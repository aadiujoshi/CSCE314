import javax.swing.JOptionPane;

public class DemoMain {
    public static void main(String[] args) {
        while (true) {
            String choice = JOptionPane.showInputDialog(
                null,
                "Concurrency Demos:\n"
              + "1) Deadlock (safe demonstration)\n"
              + "2) Deadlock (fixed ordering)\n"
              + "3) Synchronized counter\n"
              + "4) Start a new thread\n"
              + "Q) Quit",
                "Week 12 – Concurrency Demos",
                JOptionPane.QUESTION_MESSAGE
            );
            if (choice == null || choice.equalsIgnoreCase("q")) break;
            switch (choice.trim()) {
                case "1" -> new DeadlockTryDemo().run();
                case "2" -> new DeadlockFixed().run();
                case "3" -> new SyncCounterDemo().run();
                case "4" -> new ThreadStartDemo().run();
                default  -> JOptionPane.showMessageDialog(null, "Please choose 1, 2, 3, 4 or Q.");
            }
        }
    }
}