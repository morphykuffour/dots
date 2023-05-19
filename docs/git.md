Temporarily switch to a different commit
If you want to temporarily go back to it,
fool around, then come back to where you are,
all you have to do is check out the desired commit:

```bash
# This will detach your HEAD, that is, leave you with no branch checked out:
git checkout 0d1d7fc32
git checkout -b old-state 0d1d7fc32
```

