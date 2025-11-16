public abstract class LibraryItem implements Checkable {
    private static int NEXT_ID = 1000;
    private final int id;
    private final String title;
    private boolean checkedOut = false;
    private Patron checkedOutBy = null;

    public LibraryItem(String title) { this.id = NEXT_ID++; this.title = title; }
    public int getId() 
        { return id; }
    public String getTitle() 
        { return title; }

    public abstract int getLoanPeriodDays();

    @Override 
    public boolean isCheckedOut() 
        { return checkedOut; }

    @Override 
    public Patron getCheckedOutBy() 
        { return checkedOutBy; }

    @Override
    public boolean checkOut(Patron patron) {
        if (checkedOut) return false;
        checkedOut = true; checkedOutBy = patron; return true;
    }
    
    @Override
    public boolean checkIn() {
        if (!checkedOut) return false;
        checkedOut = false; checkedOutBy = null; return true;
    }

    @Override
    public String toString() {
        return String.format("%s #%d \"%s\" (loan %d days)%s",
            this.getClass().getSimpleName(), id, title, getLoanPeriodDays(),
            checkedOut ? " [OUT to " + checkedOutBy.getName() + "]" : "");
    }
}
