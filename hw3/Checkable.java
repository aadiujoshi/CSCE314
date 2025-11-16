public interface Checkable {
    boolean checkOut(Patron patron);
    boolean checkIn();
    boolean isCheckedOut();
    Patron getCheckedOutBy();
}
