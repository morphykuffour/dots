"""
Smart window creation that adapts based on current state.

If only one window exists, creates a new window with vertical split.
If multiple windows exist, cycles to the next window.
"""

def main(args):
    pass

from kittens.tui.handler import result_handler

@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    """Create a new window or cycle to next window based on current state."""
    w = boss.window_id_map.get(target_window_id)
    if w is not None:
        tab = boss.active_tab

        # If only one window, create a new split window
        if len(tab.windows) == 1:
            boss.call_remote_control(w, ('launch', '--location=vsplit', '--cwd=current'))
        else:
            # Multiple windows exist, cycle to next
            tab.next_window()
