import javax.swing.JOptionPane;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;

public class DeadlockTryDemo {
    private final ReentrantLock lockA = new ReentrantLock();
    private final ReentrantLock lockB = new ReentrantLock();

    public void run() {
        AtomicBoolean t1Blocked = new AtomicBoolean(false);
        AtomicBoolean t2Blocked = new AtomicBoolean(false);

        Thread t1 = new Thread(() -> {
            lockA.lock();
            try {
                sleep(200);
                // Try to acquire B second — might "deadlock"
                if (!tryLock(lockB, 700)) {
                    t1Blocked.set(true);
                } else {
                    try { /* work */ sleep(200); }
                    finally { lockB.unlock(); }
                }
            } finally {
                lockA.unlock();
            }
        }, "T1-A-then-B");

        Thread t2 = new Thread(() -> {
            lockB.lock();
            try {
                sleep(200);
                // Try to acquire A second — might "deadlock"
                if (!tryLock(lockA, 700)) {
                    t2Blocked.set(true);
                } else {
                    try { /* work */ sleep(200); }
                    finally { lockA.unlock(); }
                }
            } finally {
                lockB.unlock();
            }
        }, "T2-B-then-A");

        t1.start();
        t2.start();
        try { t1.join(); t2.join(); } catch (InterruptedException ignored) { }

        if (t1Blocked.get() && t2Blocked.get()) {
            JOptionPane.showMessageDialog(
                null,
                "Potential deadlock detected:\nT1 waited on B; T2 waited on A.\n" +
                "We used timed tryLock() to avoid hanging the UI.",
                "Deadlock Pattern (Safe Demo)",
                JOptionPane.WARNING_MESSAGE
            );
        } else {
            JOptionPane.showMessageDialog(
                null,
                "Both threads completed without deadlock this run.\n" +
                "Re-run to observe contention.\n" +
                "(Real deadlocks can be nondeterministic.)",
                "Deadlock Pattern (Safe Demo)",
                JOptionPane.INFORMATION_MESSAGE
            );
        }
    }

    private static boolean tryLock(ReentrantLock lock, long millis) {
        try { return lock.tryLock(millis, TimeUnit.MILLISECONDS); }
        catch (InterruptedException e) { Thread.currentThread().interrupt(); return false; }
    }

    private static void sleep(long ms) {
        try { Thread.sleep(ms); } catch (InterruptedException e) { Thread.currentThread().interrupt(); }
    }
}