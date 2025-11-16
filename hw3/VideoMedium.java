public class VideoMedium extends LibraryItem {
    private final int totalMinutes;
    private final String rating;

    public VideoMedium(String title, int totalMinutes, String rating) {
        super(title);
        this.totalMinutes = totalMinutes;
        this.rating = normalizeRating(rating);
    }

    /**
     * Validates and normalizes the rating.
     * Accepts "G" or "PG" (case-insensitive).
     * Defaults to "PG" for any invalid input with a warning.
     */
    private String normalizeRating(String input) {
        if (input == null) {
            System.out.println("Warning: null rating for video; defaulting to PG");
            return "PG";
        }
        String upper = input.toUpperCase();
        if (upper.equals("G") || upper.equals("PG")) {
            return upper;
        }
        System.out.println("Warning: invalid rating '" + input + "'; defaulting to PG");
        return "PG";
    }

    public int getTotalMinutes() {
        return totalMinutes;
    }

    public String getRating() {
        return rating;
    }

    @Override
    public int getLoanPeriodDays() {
        return 7;
    }

    @Override
    public String toString() {
        return super.toString() + String.format(" (%d min, rated %s)", totalMinutes, rating);
    }
}