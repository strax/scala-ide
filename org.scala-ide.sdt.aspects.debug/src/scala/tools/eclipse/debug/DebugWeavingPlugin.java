package scala.tools.eclipse.debug;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.BundleContext;

public class DebugWeavingPlugin extends Plugin {
	private static DebugWeavingPlugin INSTANCE;

	public static String ID = "org.scala-ide.sdt.aspects.debug"; //$NON-NLS-1$

	public DebugWeavingPlugin() {
		super();
		INSTANCE = this;
	}

	public static void logException(Throwable t) {
		INSTANCE.getLog().log(new Status(IStatus.ERROR, ID, t.getMessage(), t));
	}

	public static DebugWeavingPlugin getInstance() {
		return INSTANCE;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}
}
