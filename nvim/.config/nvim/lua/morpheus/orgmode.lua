-- orgmode settings
vim.opt.shellslash = true
vim.cmd("language en_US.utf8")
vim.opt.conceallevel = 2
vim.opt.concealcursor = "nc"

require("orgmode").setup_ts_grammar()

require("orgmode").setup({
	org_agenda_files = { "~/Dropbox/Zettelkasten/org/**/*" },
	org_default_notes_file = "~/Dropbox/Zettelkasten/org/refile.org ",
	org_deadline_warning_days = 5,
	org_agenda_start_on_weekday = 7,
	org_highlight_latex_and_related = "native",
	org_todo_keywords = { "TODO(t)", "PROGRESS(p)", "|", "DONE(d)", "REJECTED(r)" },

	mappings = {
		org = {
			org_next_visible_heading = "g}",
			org_previous_visible_heading = "g{",
		},
	},

	-- notifications = { enabled = true },

	org_agenda_templates = {
		d = {
			description = "Daily",
			template = "* Daily %U \n  %?",
			target = "~/Dropbox/Zettelkasten/org/daily.org",
			headline = "Meetings",
		},
		i = {
			description = "Thoughts",
			template = "** %?",
			target = "~/Dropbox/Zettelkasten/org/life.org",
			headline = "Thoughts",
		},
		r = {
			description = "CRandom note",
			template = "* %?",
			target = "~/Dropbox/Zettelkasten/org/random.org",
		},
		t = {
			description = "Todo",
			template = "* TODO: %?",
			target = "~/Dropbox/Zettelkasten/org/todo.org",
		},
	},
})
