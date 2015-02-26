# sbt remote control

[![Join the chat at https://gitter.im/sbt/sbt-remote-control](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/sbt/sbt-remote-control?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This is an API for controlling [sbt](http://scala-sbt.org/) from
another Scala program. It would be used for example by command line tools,
IDEs, [Activator](https://github.com/typesafehub/activator),
etc. to inspect a project build and run build tasks such as compilation.

In essence sbt-remote-control converts sbt from a command line tool into a
general backend for any kind of user interface, command line or GUI.

## Using

TODO - Let's write some documentation, for now check out the tests.


## Developing

Use [sbt](http://scala-sbt.org/) to build the project. Make sure you have an SBT launcher, and run it in the checked out directory.


Some possibly-still-relevant architectural information can be
found [on this wiki page](https://github.com/sbt/sbt/wiki/Client-server-split).

### Testing

There are two types of tests:  unit and integration.   To run all unit tests, simple run:

    sbt> test


To run the integration tests, which operate inside the sbt-launcher environment, run:

    sbt> integration-tests


### Running the terminal

You need to do the equivalent of this:

```
rm -rf ~/.sbt/boot/scala-2.10.[0-9]/com.typesafe.sbtrc/
java -jar /home/jsuereth/projects/sbt/sbt/target/sbt-launch-0.13.2-SNAPSHOT.jar @/home/jsuereth/projects/sbt/sbt-remote-control/terminal/target/resource_managed/main/sbt-client.properties
```

i.e. remove the sbtrc stuff in .sbt/boot and then launch with an
sbt launcher. Doesn't matter which sbt launcher (in theory).

### Publishing

This project currently publishes to typesafe's ivy-releases repository.  To cut a distribution, first tag the project via:


    git tag -u <your PGP key> v<version>


Then start or reload sbt:


    sbt> reload


You should see the tagged version of the project if you run the `version` command in sbt.  Now just run `publish-local` and the release is out.


### Trying out the Terminal

First, make sure to publish a local instance of the server

    sbt> publishLocal

Next, create a staged distribution of the terminal client

    sbt> stage

Finally, look in the `terminal/target/universal/stage/bin` directory for the `sbt-terminal` script which will let you play with the new sbt-terminal implementation.

## License

This software is licensed under the [Apache 2 license](http://www.apache.org/licenses/LICENSE-2.0).
