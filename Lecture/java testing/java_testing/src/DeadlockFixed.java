import javax.swing.JOptionPane;
import java.util.concurrent.locks.ReentrantLock;

public class DeadlockFixed {
    private final ReentrantLock lockA = new ReentrantLock();
    private final ReentrantLock lockB = new ReentrantLock();

    public void run() {
        // Global lock ordering: ALWAYS acquire A then B.
        Runnable work = () -> {
            lockA.lock();
            try {
                sleep(150);
                lockB.lock();
                try {
                    // critical section
                    sleep(150);
                } finally {
                    lockB.unlock();
                }
            } finally {
                lockA.unlock();
            }
        };

        Thread t1 = new Thread(work, "Fixed-1");
        Thread t2 = new Thread(work, "Fixed-2");
        t1.start(); t2.start();
        try { t1.join(); t2.join(); } catch (InterruptedException ignored) { }

        JOptionPane.showMessageDialog(
            null,
            "Both threads respected a consistent lock order (A→B).\n" +
            "No circular wait ⇒ no deadlock.",
            "Deadlock Fixed (Lock Ordering)",
            JOptionPane.INFORMATION_MESSAGE
        );
    }

    private static void sleep(long ms) {
        try { Thread.sleep(ms); } catch (InterruptedException e) { Thread.currentThread().interrupt(); }
    }
}