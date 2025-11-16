import java.io.*;
import java.util.*;

public class Main {
    private static Library library;
    private static Scanner scanner;

    public static void main(String[] args) {
        scanner = new Scanner(System.in);

        if (args.length < 2) {
            System.out.println("Usage: java Main <patrons_file> <items_file>");
            System.exit(1);
        }

        try {
            library = loadLibraryFromFiles(args[0], args[1]);
            System.out.println("Library loaded successfully.\n");
            menuLoop();
        } catch (IOException e) {
            System.err.println("Error loading library: " + e.getMessage());
            System.exit(1);
        }
    }

    public static Library loadLibraryFromFiles(String patronFile, String itemFile) throws IOException {
        Library lib = new Library();

        // Load patrons
        try (BufferedReader br = new BufferedReader(new FileReader(patronFile))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("#")) continue;

                String[] parts = line.split(",");
                if (parts.length != 2) {
                    System.out.println("Warning: malformed patron line: " + line);
                    continue;
                }

                String name = parts[0].trim();
                boolean eligible = parts[1].trim().equalsIgnoreCase("y");
                lib.addPatron(new Patron(name, eligible));
            }
        }

        // Load items
        try (BufferedReader br = new BufferedReader(new FileReader(itemFile))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("#")) continue;

                String[] parts = line.split(",");
                if (parts.length == 0) {
                    System.out.println("Warning: empty line");
                    continue;
                }

                String type = parts[0].trim().toUpperCase();
                try {
                    LibraryItem item = parseItem(type, parts);
                    if (item != null) {
                        lib.addItem(item);
                    }
                } catch (Exception e) {
                    System.out.println("Warning: error parsing line '" + line + "': " + e.getMessage());
                }
            }
        }

        return lib;
    }

    /**
     * Parses a CSV line and creates the appropriate LibraryItem.
     */
    private static LibraryItem parseItem(String type, String[] parts) {
        switch (type) {
            case "HARDBACK":
                if (parts.length != 4) throw new IllegalArgumentException("Expected 4 fields for HARDBACK");
                return new Hardback(
                    parts[1].trim(),
                    parts[2].trim(),
                    Integer.parseInt(parts[3].trim())
                );

            case "PAPERBACK":
                if (parts.length != 4) throw new IllegalArgumentException("Expected 4 fields for PAPERBACK");
                return new Paperback(
                    parts[1].trim(),
                    parts[2].trim(),
                    Integer.parseInt(parts[3].trim())
                );

            case "PERIODICAL":
                if (parts.length != 3) throw new IllegalArgumentException("Expected 3 fields for PERIODICAL");
                return new Periodical(parts[1].trim(), parts[2].trim());

            case "BOOK_ON_TAPE":
                if (parts.length != 4) throw new IllegalArgumentException("Expected 4 fields for BOOK_ON_TAPE");
                return new BookOnTape(
                    parts[1].trim(),
                    parts[2].trim(),
                    Integer.parseInt(parts[3].trim())
                );

            case "VIDEO":
                if (parts.length != 4) throw new IllegalArgumentException("Expected 4 fields for VIDEO");
                return new VideoMedium(
                    parts[1].trim(),
                    Integer.parseInt(parts[2].trim()),
                    parts[3].trim()
                );

            default:
                throw new IllegalArgumentException("Unknown item type: " + type);
        }
    }

    /**
     * Main menu loop.
     */
    private static void menuLoop() {
        while (true) {
            printMenu();
            System.out.print("Choice: ");
            String choice = scanner.nextLine().trim();

            switch (choice) {
                case "1":
                    printInventory();
                    break;
                case "2":
                    interfaceDemo();
                    break;
                case "3":
                    printPatrons();
                    break;
                case "4":
                    checkout();
                    break;
                case "5":
                    checkin();
                    break;
                case "6":
                    togglePatronEligibility();
                    break;
                case "7":
                    System.out.println("Goodbye!");
                    return;
                default:
                    System.out.println("Invalid choice. Please try again.\n");
            }
        }
    }

    private static void printMenu() {
        System.out.println("=== Library Menu ===");
        System.out.println("1) Inventory");
        System.out.println("2) Interface Demo");
        System.out.println("3) Patrons");
        System.out.println("4) Checkout");
        System.out.println("5) Checkin");
        System.out.println("6) Toggle Patron Eligibility");
        System.out.println("7) Quit");
    }

    private static void printInventory() {
        System.out.println("\n=== Inventory ===");
        for (LibraryItem item : library.getItems()) {
            System.out.println(item);
        }
        System.out.println();
    }

    private static void interfaceDemo() {
        System.out.println("\n=== Interface Demo (Checkable) ===");
        for (LibraryItem item : library.getItems()) {
            Checkable c = (Checkable) item;
            System.out.printf("%s: checkedOut=%b, loanPeriod=%d days%n",
                item.getTitle(), c.isCheckedOut(), item.getLoanPeriodDays());
        }
        System.out.println();
    }

    private static void printPatrons() {
        System.out.println("\n=== Patrons ===");
        for (Patron p : library.getPatrons()) {
            System.out.println(p);
            if (!p.getLoans().isEmpty()) {
                System.out.println("  Loans:");
                for (LibraryItem li : p.getLoans()) {
                    System.out.println("   > " + li);
                }
            }
        }
        System.out.println();
    }

    private static void checkout() {
        System.out.print("\nEnter patron id: ");
        int patronId = readInt();
        System.out.print("Enter item id: ");
        int itemId = readInt();

        var patron = findPatron(patronId);
        var item = findItem(itemId);

        if (patron == null) {
            System.out.println("Checkout failed: Patron not found.\n");
            return;
        }
        if (item == null) {
            System.out.println("Checkout failed: Item not found.\n");
            return;
        }
        if (!patron.isEligibleForCheckout()) {
            System.out.println("Checkout failed: Patron not eligible to checkout.\n");
            return;
        }

        boolean success = library.checkout(patronId, itemId);
        if (success) {
            System.out.printf("Checkout success: %s → Patron %d (%s)%n%n",
                item.getTitle(), patronId, patron.getName());
        } else {
            System.out.println("Checkout failed: Item already checked out or patron has too many loans.\n");
        }
    }

    private static void checkin() {
        System.out.print("\nEnter item id: ");
        int itemId = readInt();

        var item = findItem(itemId);
        if (item == null) {
            System.out.println("Checkin failed: Item not found.\n");
            return;
        }

        boolean success = library.checkin(itemId);
        if (success) {
            System.out.printf("Checkin success: %s%n%n", item.getTitle());
        } else {
            System.out.println("Checkin failed: Item is not checked out.\n");
        }
    }

    private static void togglePatronEligibility() {
        System.out.print("\nEnter patron id: ");
        int patronId = readInt();

        var patron = findPatron(patronId);
        if (patron == null) {
            System.out.println("Patron not found.\n");
            return;
        }

        patron.setEligibleForCheckout(!patron.isEligibleForCheckout());
        System.out.printf("Patron eligibility toggled: now eligible=%b%n%n", 
            patron.isEligibleForCheckout());
    }

    private static Patron findPatron(int id) {
        var opt = library.getPatrons().stream()
            .filter(p -> p.getId() == id)
            .findFirst();
        return opt.orElse(null);
    }

    private static LibraryItem findItem(int id) {
        var opt = library.getItems().stream()
            .filter(i -> i.getId() == id)
            .findFirst();
        return opt.orElse(null);
    }

    private static int readInt() {
        try {
            return Integer.parseInt(scanner.nextLine().trim());
        } catch (NumberFormatException e) {
            System.out.println("Invalid input. Please enter a number.");
            return -1;
        }
    }
}