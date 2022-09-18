# Preamble
[Exercism](https://exercism.org) is a great tool for learning new languages or diving deeper into familiar ones!

It offers not just a nice web editor but also a CLI if you want to use your local editor of choice. This package aims to streamline the latter, via Emacs!

# Quick Start
- Invoke the command `exercism`. It will popup a [transient](https://github.com/magit/transient) menu (similar to our beloved `magit`!)

- `Configure` - (once off) [Get your API token](https://exercism.org/settings/api_cli) and pop it into the prompt
![configure](./demos/menu.png)
- `Set current track` - Choose the track that you want to do exercises for.
  - This might take a few minutes the first time because it "initializes" the track locally. Subsequent invocations will be instant.
<video src="./demos/set-track.mp4" width=300 />
- `Open an exercise` - Time to get into it! Note that some exercises may still be unlocked, yet "select-able" (see the "Known Limitations" section).
- `Submit` - Submits the current directory as a solution. Note that to mark the exercise as "completed", you'll have to do it via the web app.
  This is what `Submit (then open in browser)` is for. It automatically opens the exercise in the browser after submission.

# Known Limitations
- Registering yourself to a track isn't currently supported. You'll have to do this via [their web app](https://exercism.org/tracks), sorry!
- The exercises list that you choose from, also includes "not yet unlocked" ones. This is due to the the fact that the CLI doesn't support listing out exercises.

# Contributing
- All PRs, suggestions, complaints and anything in between are welcome!