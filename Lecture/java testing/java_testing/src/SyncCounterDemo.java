import javax.swing.JOptionPane;
import java.util.ArrayList;
import java.util.List;

public class SyncCounterDemo {

    static class Counter {
        private int value = 0;
        void incUnsafe() { value++; }                // not synchronized
        synchronized void incSafe() { value++; }     // synchronized
        int get() { return value; }
    }

    public void run() {
        int threads = askInt("How many threads?", 8);
        int increments = askInt("How many increments per thread?", 100_000);

        // UNSAFE PASS
        Counter unsafe = new Counter();
        long t0 = System.currentTimeMillis();
        runWorkers(threads, increments, unsafe, false);
        long t1 = System.currentTimeMillis();
        int expected = threads * increments;

        // SAFE PASS
        Counter safe = new Counter();
        long t2 = System.currentTimeMillis();
        runWorkers(threads, increments, safe, true);
        long t3 = System.currentTimeMillis();

        JOptionPane.showMessageDialog(
            null,
            "UNSAFE (no sync): " + unsafe.get() + " / expected " + expected + "  in " + (t1 - t0) + "ms\n" +
            "SAFE   (synchronized): " + safe.get()   + " / expected " + expected + "  in " + (t3 - t2) + "ms\n\n" +
            "Race conditions corrupt the UNSAFE count.\n" +
            "Synchronization restores correctness (with some overhead).",
            "Synchronized Counter Demo",
            JOptionPane.INFORMATION_MESSAGE
        );
    }

    private static void runWorkers(int threads, int increments, Counter c, boolean safe) {
        List<Thread> list = new ArrayList<>();
        for (int i = 0; i < threads; i++) {
            Thread t = new Thread(() -> {
                for (int k = 0; k < increments; k++) {
                    if (safe) c.incSafe(); else c.incUnsafe();
                }
            });
            list.add(t);
            t.start();
        }
        for (Thread t : list) {
            try { t.join(); } catch (InterruptedException e) { Thread.currentThread().interrupt(); }
        }
    }

    private static int askInt(String prompt, int fallback) {
        String s = JOptionPane.showInputDialog(null, prompt + " (default " + fallback + "):");
        if (s == null || s.isBlank()) return fallback;
        try { return Integer.parseInt(s.trim()); } catch (NumberFormatException e) { return fallback; }
    }
}