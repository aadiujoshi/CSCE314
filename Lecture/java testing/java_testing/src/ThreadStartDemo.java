import javax.swing.JOptionPane;

public class ThreadStartDemo {
    public void run() {
        Thread t = new Thread(() -> {
            String name = JOptionPane.showInputDialog(null, "What's your name?");
            if (name == null || name.isBlank()) {
                JOptionPane.showMessageDialog(null, "Hello from another thread!");
            } else {
                JOptionPane.showMessageDialog(null, "Hello, " + name + " — from another thread!");
            }
        }, "HelloThread");

        t.start();
        JOptionPane.showMessageDialog(
            null,
            "Started Thread: " + t.getName() + "\nThe main thread stays responsive.",
            "Thread Start Demo",
            JOptionPane.INFORMATION_MESSAGE
        );
        try { t.join(); } catch (InterruptedException ignored) {}
    }
}