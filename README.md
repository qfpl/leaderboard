# Leaderboard

Leaderboard is a small web service to record the results of matches between players, and provide
ranking of players.

## State machine testing

This project was used to build up examples for [@ajmccluskey's talk](https://qfpl.io/talks/) on
state machine testing at [YOW! Lambda Jam](http://lambdajam.yowconference.com.au/) in 2018. If you
are looking for the version of the code used in that talk, with a separate module for each example,
then checkout the [`ylj2018-20180522`
tag](https://github.com/qfpl/leaderboard/tree/ylj2018-20180522).

## Building

Currently, leaderboard depends on a few versions of packages that aren't available on hackage. There
are two reasons this is the case:

1. Development was started before `beam` packages were available on hackage, and the code has not
   yet been updated to work with newer versions.
2. Two of the dependencies have been forked and modified, and the pull requests on those packages
   have not all been merged.

This is all taken care of with nix, so the following should get you a build of the package.

```
nix-build
```

Similarly, to work on the package, one may use `nix-shell` to be put in an environment where the
package can be built.

```
$ nix-shell
$ cabal new-build
```

If one desperately wants to build this package without nix, one could clone the relevant
repositories (or add them as git submodules), and then use a `cabal.project` file to make them
available to `cabal new-*`.


