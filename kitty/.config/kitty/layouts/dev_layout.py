"""
Create a development layout: editor on the left, shell on the right.

This creates a two-pane vertical split layout optimized for development:
- Left pane: Opens in the current directory (for editor/code)
- Right pane: Opens in the current directory (for shell commands)
"""

def main(args):
    pass

from kittens.tui.handler import result_handler

@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    """Create a dev layout with editor and shell side by side."""
    w = boss.window_id_map.get(target_window_id)
    if w is not None:
        # Close all other windows in this tab
        boss.call_remote_control(w, ('close-window', '--match=state:parent_active'))

        # Create new window (left pane)
        boss.call_remote_control(w, ('launch', '--cwd=current'))

        # Create vertical split (right pane)
        boss.call_remote_control(w, ('launch', '--location=vsplit', '--cwd=current'))
