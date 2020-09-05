# Hackage Search Backend Design

**DEFINITION:**
  "Hackage Directory" is the directory specified with the '--hackage' flag.

The backend has a persistent, shared, mutable state, which it needs to manage:
the Hackage Directory. It contains packages downloaded from Hackage.

1. It's persistent to avoid re-downloading large amounts of data.
2. It's shared because each concurrent search request needs access to it.
3. It's mutable to provide up-to-date package contents.

And if the combination of words "persistent", "shared", and "mutable" sounds
like a recipe for disaster to you, that's because it is. So we must be
especially careful to design the backend in such a way that it doesn't end up
in an invalid state.

The process may crash at any time, for example due to electricity outage. So
the state on disk must be valid at all times. If the process is restarted, it
must be able to continue.

**DEFINITION**:
  "Validity Invariant" postulates that the data in the Hackage Directory is
  correct and complete at all times.

In order to maintain the Validity Invariant, we have to make use of atomic file
system operations, in particular 'System.Posix.Files.rename'.

Also, it's possible that the user will run multiple search servers
concurrently, specifying the same Hackage Directory. So the process should not
assume exclusive access to the directory.
