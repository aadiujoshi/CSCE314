public class Library {
    private final java.util.List<LibraryItem> items = new java.util.ArrayList<>();
    private final java.util.List<Patron> patrons = new java.util.ArrayList<>();

    public void addItem(LibraryItem item) { 
        items.add(item); 
    }
    
    public void addPatron(Patron patron) { 
        patrons.add(patron); 
    }

    public java.util.Optional<LibraryItem> findItem(int id) {
        return items.stream().filter(i -> i.getId() == id).findFirst();
    }
    
    public java.util.Optional<Patron> findPatron(int id) {
        return patrons.stream().filter(p -> p.getId() == id).findFirst();
    }

    public boolean checkout(int patronId, int itemId) {
        var p = findPatron(patronId);
        var i = findItem(itemId);
        if (p.isEmpty() || i.isEmpty()) return false;
        return p.get().borrow(i.get());
    }

    public boolean checkin(int itemId) {
        var itemOpt = findItem(itemId);
        if (itemOpt.isEmpty()) return false;
        var item = itemOpt.get();
        var borrower = item.getCheckedOutBy();
        if (borrower != null && borrower.returnItem(item)) return true;
        return item.checkIn(); // safety
    }

    public void printInventory() {
        System.out.println("=== Inventory ===");
        items.forEach(System.out::println);
    }
    
    public void printPatrons() {
        System.out.println("=== Patrons ===");
        patrons.forEach(p -> {
            System.out.println(p);
            if (!p.getLoans().isEmpty()) {
                System.out.println("  Loans:");
                p.getLoans().forEach(li -> System.out.println("   > " + li));
            }
        });
    }

    public java.util.List<LibraryItem> getItems() { 
        return items; 
    }
    
    public java.util.List<Patron> getPatrons() { 
        return patrons; 
    }
}