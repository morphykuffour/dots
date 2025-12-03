"""
Move the current pane to a new OS window.

This kitten moves the currently focused window (pane) to a new OS window,
preserving the working directory.
"""

def main(args):
    pass

from kittens.tui.handler import result_handler

@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    """Move the current window to a new OS window."""
    w = boss.window_id_map.get(target_window_id)
    if w is not None:
        # Launch new OS window with current directory
        boss.call_remote_control(w, ('launch', '--type=os-window', '--cwd=current'))
        # Close the original window
        boss.call_remote_control(w, ('close-window', '--self'))
