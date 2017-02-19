# Greek phonology library

## What it is

`gsphone` is a cross-platform library for phonological maniuplation of `GreekString` objects .

## Current version: 1.0.1

Status: experimental.  [Release notes](releases.md)


## License

[GPL 3.0](http://www.opensource.org/licenses/gpl-3.0.html)

## Using, building, testing

`gsphone` is compiled for both the JVM and ScalaJS using scala versions 2.10, 2.11 and 2.12.  Binaries for all platforms are available from jcenter.

If you are using sbt, include `Resolver.jcenterRepo` in your list of resolvers

    resolvers += Resolver.jcenterRepo

and add this to your library dependencies:

    "edu.holycross.shot" %%% "gsphone" % VERSION


For maven, ivy or gradle equivalents, refer to <https://bintray.com/neelsmith/maven/gsphone>.

To build from source and test, use normal sbt commands (`compile`, `test` ...).
