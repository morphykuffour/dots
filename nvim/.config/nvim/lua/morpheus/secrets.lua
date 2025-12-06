local M = {}

local function file_exists(path)
  if not path or path == '' then
    return false
  end
  return vim.loop.fs_stat(path) ~= nil
end

local function trim(s)
  return (s:gsub('^%s+', ''):gsub('%s+$', ''))
end

local function dbg(msg)
  if vim.env.NVIM_AI_DEBUG == '1' then
    vim.schedule(function()
      vim.notify(msg, vim.log.levels.INFO)
    end)
  end
end

local function pick_decryptor()
  local function resolve_exe(name, env_var)
    local from_env = os.getenv(env_var)
    if from_env and file_exists(vim.fn.expand(from_env)) then
      return vim.fn.expand(from_env)
    end
    local by_path = vim.fn.exepath(name)
    if by_path and by_path ~= '' and file_exists(by_path) then
      return by_path
    end
    local user = os.getenv('USER') or ''
    local nix_path = '/etc/profiles/per-user/' .. user .. '/bin/' .. name
    if file_exists(nix_path) then
      return nix_path
    end
    return nil
  end

  local rage_bin = resolve_exe('rage', 'RAGE_BIN')
  if rage_bin then
    return 'rage', rage_bin
  end
  local age_bin = resolve_exe('age', 'AGE_BIN')
  if age_bin then
    return 'age', age_bin
  end
  return nil, nil
end

-- Decrypt OPENAI_API_KEY using rage/age if env var is not set
-- Defaults:
--   enc file: ~/nix/secrets/openai.age
--   identity: $AGE_IDENTITY_FILE or ~/.ssh/id_ed25519
function M.bootstrap_openai_from_rage(opts)
  if vim.env.OPENAI_API_KEY and vim.env.OPENAI_API_KEY ~= '' then
    return
  end

  opts = opts or {}
  local home = os.getenv('HOME') or vim.fn.expand('~')
  local enc_file = opts.enc_file or os.getenv('OPENAI_AGE_FILE') or (home .. '/nix/secrets/openai.age')
  local id_file = opts.identity_file or os.getenv('AGE_IDENTITY_FILE') or (home .. '/.ssh/id_ed25519')

  if not file_exists(enc_file) then
    dbg('openai.age not found at ' .. enc_file)
    return
  end
  if not file_exists(id_file) then
    dbg('AGE identity not found at ' .. id_file)
    return
  end

  local dec, dec_path = pick_decryptor()
  if not dec then
    dbg('Neither rage nor age found in PATH')
    return
  end

  local cmd
  if dec == 'rage' then
    -- Explicitly write to stdout to mirror shell sanity check
    cmd = { dec_path, '-d', '-i', id_file, '-o', '-', enc_file }
  else
    cmd = { dec_path, '-d', '-i', id_file, '-o', '-', enc_file }
  end

  local ok, out = pcall(vim.fn.system, cmd)
  if not ok then
    dbg('decrypt command failed to execute: ' .. table.concat(cmd, ' '))
    return
  end
  if vim.v.shell_error ~= 0 then
    dbg('decrypt command returned non-zero exit: ' .. tostring(vim.v.shell_error) .. ' for: ' .. table.concat(cmd, ' '))
    return
  end

  local value = trim(out or '')
  if value ~= '' then
    vim.env.OPENAI_API_KEY = value
    dbg('OPENAI_API_KEY populated from ' .. dec)
  else
    dbg('decrypted value was empty')
  end
end

function M.test(opts)
  local dec, dec_path = pick_decryptor()
  local home = os.getenv('HOME') or vim.fn.expand('~')
  local enc_file = (opts and opts.enc_file) or os.getenv('OPENAI_AGE_FILE') or (home .. '/nix/secrets/openai.age')
  local id_file = (opts and opts.identity_file) or os.getenv('AGE_IDENTITY_FILE') or (home .. '/.ssh/id_ed25519')

  local facts = {
    'Decryptor: ' .. (dec or 'none'),
    'Decryptor path: ' .. (dec_path or 'n/a'),
    'Encrypted file: ' .. enc_file,
    'Identity file: ' .. id_file,
    'Encrypted exists: ' .. tostring(file_exists(enc_file)),
    'Identity exists: ' .. tostring(file_exists(id_file)),
  }

  M.bootstrap_openai_from_rage(opts)

  if vim.env.OPENAI_API_KEY and vim.env.OPENAI_API_KEY ~= '' then
    table.insert(facts, 'Result: OPENAI_API_KEY set')
    vim.schedule(function()
      vim.notify(table.concat(facts, '\n'), vim.log.levels.INFO)
    end)
  else
    table.insert(facts, 'Result: FAILED to set OPENAI_API_KEY')
    vim.schedule(function()
      vim.notify(table.concat(facts, '\n'), vim.log.levels.ERROR)
    end)
  end
end

return M


