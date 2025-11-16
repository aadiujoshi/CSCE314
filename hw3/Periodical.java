public class Periodical extends LibraryItem {
    private final String issue;
    public Periodical(String title, String issue) { super(title); this.issue = issue; }
    public String getIssue() { return issue; }
    @Override public int getLoanPeriodDays() { return 7; }
    @Override public String toString() { return super.toString() + String.format(" — issue %s", issue); }
}
