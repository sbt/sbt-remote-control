package com.typesafe.sbtrc.agent;

import java.lang.instrument.*;
import java.util.concurrent.atomic.AtomicReference;

public class ExampleAgent {
  private static AtomicReference<String> agentString = new AtomicReference<String>(null);
  public static void premain(String args, Instrumentation inst) {
    agentString.set("running");
  }
  public static String getAgentString() {
    return agentString.get();
  }
}