public class Hardback extends Book {
    public Hardback(String title, String author, int pages) 
        { super(title, author, pages); }
    @Override public int getLoanPeriodDays() { return 21; }
}
