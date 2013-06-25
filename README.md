# sbt remote control

This is a project for starting up and controlling a pool of sbt processes remotely (via a different process).


## Using

TODO - Let's write some documentation, for now check out the tests.


## Developing

This project uses [sbt 0.12](http://scala-sbt.org/). Make sure you have an SBT launcher, and run it in the checked out directory.

### Testing

There are two types of tests:  unit and integration.   To run all unit tests, simple run:

    sbt> test


To run the integration tests, which operate inside the sbt-launcher environment, run:

    sbt> integration-tests



### Publishing

This project currently publishes to typesafe's ivy-releases repository.  To cut a distribution, first tag the project via:


    git tag -u <your PGP key> v<version>


Then start or reload sbt:


    sbt> reload


You should see the tagged version of the project if you run the `version` command in sbt.  Now just run `publish-local` and the release is out.


## License

This software is licensed under the [Apache 2 license](http://www.apache.org/licenses/LICENSE-2.0).
