package com.typesafe.sbtrc.properties;

import java.util.Properties;

/**
 * Access to all the "global" environment/configuration variables for the sbt-rc
 * project.
 */
public final class SbtRcProperties {

	private static Properties loadProperties() {
		Properties props = new Properties();
		java.io.InputStream in = SbtRcProperties.class
				.getResourceAsStream("sbtrc.properties");
		try {
			props.load(in);
		} catch (java.io.IOException e) {
			throw new RuntimeException(e);
		} finally {
			try {
				in.close();
			} catch (java.io.IOException e) {
				throw new RuntimeException(e);
			}
		}
		return props;
	}

	// Note - this is locked, thanks to classloader magic, and we never
	// modify, so it's safe to share.
	private static Properties props = loadProperties();

	/**
	 * Checks the system properties, before the environment, before the hard
	 * coded defaults.
	 */
	private static String getProperty(String name) {
		String value = System.getProperty(name);
		if (value == null) {
			value = System.getenv("sbtrc." + name);
		}
		if (value == null) {
			value = System.getenv("SBTRC_"
					+ name.replace('.', '_').toUpperCase());
		}
		if (value == null) {
			value = props.getProperty(name);
		}
		return value;
	}

	/** Looks up a property value, and parses its value as appropriate. */
	private static String lookupOr(String name, String defaultValue) {
		String value = getProperty(name);
		if (value == null) {
			value = defaultValue;
		}
		return value;
	}

	/** The version of sbt our shims support. */
	public static String SBT_VERSION() {
		return props.getProperty("sbt.version");
	}

	/** The version of scala our sbt shims support. */
	public static String SBT_SCALA_VERSION() {
		return props.getProperty("sbt.scala.version");
	}

	/** The version of sbt rc we're running. */
	public static String APP_VERSION() {
		return props.getProperty("app.version");
	}

	public static String SBT_XMX() {
		return lookupOr("sbt.Xmx", "512M");
	}

	public static String SBT_PERMSIZE() {
		return lookupOr("sbt.PermSize", "128M");
	}
}