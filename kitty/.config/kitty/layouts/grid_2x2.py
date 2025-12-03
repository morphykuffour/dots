"""
Create a 2x2 grid layout.

This creates a 2x2 grid layout:
- Top-left, top-right
- Bottom-left, bottom-right
"""

def main(args):
    pass

from kittens.tui.handler import result_handler

@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    """Create a 2x2 grid layout."""
    w = boss.window_id_map.get(target_window_id)
    if w is not None:
        # Close all other windows in this tab
        boss.call_remote_control(w, ('close-window', '--match=state:parent_active'))

        # Create top-left pane
        boss.call_remote_control(w, ('launch', '--cwd=current'))

        # Create top-right pane
        boss.call_remote_control(w, ('launch', '--location=vsplit', '--cwd=current'))

        # Focus back to first window to create bottom row
        boss.call_remote_control(w, ('focus-window', '--match=num:1'))

        # Create bottom-left pane
        boss.call_remote_control(w, ('launch', '--location=hsplit', '--cwd=current'))

        # Create bottom-right pane
        boss.call_remote_control(w, ('launch', '--location=vsplit', '--cwd=current'))

        # Focus back to top-left pane
        boss.call_remote_control(w, ('focus-window', '--match=num:1'))
