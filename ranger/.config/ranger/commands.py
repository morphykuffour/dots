from threading import Thread
from ranger.api.commands import Command

class edir(Command):
    '''
    :edir [file|dir]

    Run edir on the selected file or dir.
    Default argument is current dir.
    '''
    def execute(self):
        self.fm.run('edir -q ' + self.rest(1))
    def tab(self, tabnum):
        return self._tab_directory_content()

class fzf_select(Command):
    """
    :fzf_select

    Find a file using fzf.

    With a prefix argument select only directories.

    See: https://github.com/junegunn/fzf
    """
    def execute(self):
        import subprocess
        import os.path
        fzf = self.fm.execute_command("fzf +m", universal_newlines=True, stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.rstrip('\n'))
            if os.path.isdir(fzf_file):
                self.fm.cd(fzf_file)
            else:
                self.fm.select_file(fzf_file)


class dragon(Command):

    def execute(self):
        th = Thread(target=self.dragondaemon, daemon=True)
        th.start()
        th.join()

    def dragondaemon(self):
        arguments = 'kitty --class dragon-term -e dragon-daemon {}'.format(" ".join(self.args[1:]))
        self.fm.execute_command(arguments)
