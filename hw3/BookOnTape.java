public class BookOnTape extends LibraryItem {
    private final String author;
    private final int totalMinutes;

    public BookOnTape(String title, String author, int totalMinutes) {
        super(title);
        this.author = author;
        this.totalMinutes = totalMinutes;
    }

    public String getAuthor() {
        return author;
    }

    public int getTotalMinutes() {
        return totalMinutes;
    }

    @Override
    public int getLoanPeriodDays() {
        return 14;
    }

    @Override
    public String toString() {
        return super.toString() + String.format(" by %s (%d min)", author, totalMinutes);
    }
}