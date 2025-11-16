public abstract class Book extends LibraryItem {
    private final String author;
    private final int pages;
    public Book(String title, String author, int pages) 
        { super(title); this.author = author; this.pages = pages; }
    public String getAuthor() 
        { return author; }
    public int getPages() 
        { return pages; }
    @Override 
    public String toString() 
        { return super.toString() + String.format(" by %s (%d pages)", author, pages); }
}
