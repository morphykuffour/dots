"""
Create a three-column layout.

This creates a three-pane layout:
- Left pane: Main work area
- Middle pane: Secondary work area
- Right pane: Terminal/monitoring
"""

def main(args):
    pass

from kittens.tui.handler import result_handler

@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    """Create a three-column layout."""
    w = boss.window_id_map.get(target_window_id)
    if w is not None:
        # Close all other windows in this tab
        boss.call_remote_control(w, ('close-window', '--match=state:parent_active'))

        # Create first column
        boss.call_remote_control(w, ('launch', '--cwd=current'))

        # Create second column
        boss.call_remote_control(w, ('launch', '--location=vsplit', '--cwd=current'))

        # Create third column
        boss.call_remote_control(w, ('launch', '--location=vsplit', '--cwd=current'))
