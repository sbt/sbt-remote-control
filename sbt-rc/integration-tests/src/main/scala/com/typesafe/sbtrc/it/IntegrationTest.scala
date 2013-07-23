package com.typesafe.sbtrc
package it

/** Base class for integration tests. */
abstract class IntegrationTest extends DelayedInit with xsbti.AppMain {
  // Junk to make delayed init work.
  private var _config: xsbti.AppConfiguration = null
  private var _test: () => Unit = null
  final def delayedInit(x: => Unit): Unit = _test = () => x

  /** Returns the current sbt launcher configuration for the test. */
  final def configuration: xsbti.AppConfiguration = _config

  def repositories = configuration.provider.scalaProvider.launcher.appRepositories
  // Runs our test, we hardcode this to return success in the absence of failure, so we can use
  // classic exceptions to fail an integration test.
  final def run(configuration: xsbti.AppConfiguration): xsbti.MainResult =
    try withContextClassloader {
      _config = configuration
      _test()
      // IF we don't throw an exception, we've succeeded
      Success
    } catch {
      case t: Exception =>
        t.printStackTrace()
        Failure
    }
}
