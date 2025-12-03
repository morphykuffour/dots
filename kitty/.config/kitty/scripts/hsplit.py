"""
Create a new window with horizontal split (new pane below current).

This kitten creates a new window below the current one,
preserving the working directory.
"""

def main(args):
    pass

from kittens.tui.handler import result_handler

@result_handler(no_ui=True)
def handle_result(args, answer, target_window_id, boss):
    """Create a new window with horizontal split."""
    w = boss.window_id_map.get(target_window_id)
    if w is not None:
        boss.call_remote_control(w, ('launch', '--location=hsplit', '--cwd=current'))
