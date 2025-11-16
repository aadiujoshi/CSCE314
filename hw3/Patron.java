public class Patron {
    private static int NEXT_ID = 1;
    private static final int MAX_LOANS = 3;

    private final int id;
    private final String name;
    private boolean eligibleForCheckout;
    private final java.util.List<LibraryItem> loans = new java.util.ArrayList<>();

    public Patron(String name) {
        this(name, true);
    }

    public Patron(String name, boolean eligibleForCheckout) {
        this.id = NEXT_ID++;
        this.name = name;
        this.eligibleForCheckout = eligibleForCheckout;
    }

    public int getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public boolean isEligibleForCheckout() {
        return eligibleForCheckout;
    }

    public void setEligibleForCheckout(boolean eligible) {
        this.eligibleForCheckout = eligible;
    }

    public java.util.List<LibraryItem> getLoans() {
        return loans;
    }

    public boolean canBorrow() {
        return eligibleForCheckout && loans.size() < MAX_LOANS;
    }

    public boolean borrow(LibraryItem item) {
        if (!eligibleForCheckout) return false;
        if (!canBorrow()) return false;
        if (item.checkOut(this)) {
            loans.add(item);
            return true;
        }
        return false;
    }

    public boolean returnItem(LibraryItem item) {
        if (loans.contains(item) && item.checkIn()) {
            loans.remove(item);
            return true;
        }
        return false;
    }

    @Override
    public String toString() {
        return String.format("Patron #%d %s (eligible=%b, loans: %d)", 
            id, name, eligibleForCheckout, loans.size());
    }
}