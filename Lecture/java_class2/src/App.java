import javax.swing.JOptionPane;
import javax.swing.UIManager;

public class App {
    public static void main(String[] args) throws Exception {
        UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
        String name = JOptionPane.showInputDialog("What is your name");

        Hello.greet();

        String[] colors = new String[]{
            "red",
            "orange"
        };

        String res = (String) JOptionPane.showInputDialog(
            null, 
            "Favorite color?", 
            "color",
            0,
            null,
            colors, 
            colors[0]
        );

        System.err.println(name + "  " + res);
    }
}
