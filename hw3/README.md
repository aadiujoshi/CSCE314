Program Usage:
Run the program using:
java Main <patrons_file> <items_file>

Example:
java Main patrons.txt items.txt

The program takes two input files as arguments. It loads all patrons and items from these files before starting the main menu.

Patrons File Format (CSV):
Each line contains a patron’s name and eligibility flag, separated by a comma.
Format: Name,EligibilityFlag

Name is the patron’s full name.

EligibilityFlag is 'y' or 'Y' if the patron can check out items; anything else means ineligible.

Blank lines and lines starting with # are ignored.

Example:
Alice,y
Bob,n
Jordan,Y
Priya,N

Items File Format (CSV):
The format depends on the type of library item.
HARDBACK,Title,Author,Pages
PAPERBACK,Title,Author,Pages
PERIODICAL,Title,IssueLabel
BOOK_ON_TAPE,Title,Author,TotalMinutes
VIDEO,Title,TotalMinutes,Rating

Rating must be G or PG (not case-sensitive). Invalid ratings are changed to PG with a warning.

Blank lines and comments (#) are ignored.

Leading or trailing spaces are removed.

Example:
HARDBACK,Effective Java,Joshua Bloch,416
PAPERBACK,Clean Code,Robert C. Martin,464
PERIODICAL,ACM Communications,2025-10
BOOK_ON_TAPE,The Pragmatic Programmer,Andrew Hunt,720
VIDEO,Toy Story,81,G
VIDEO,Finding Nemo,100,PG

Design Notes:

BookOnTape extends LibraryItem (not Book) because audiobooks don’t have pages and are stored differently.
Book.java requires a pages field, so inheriting directly from LibraryItem avoids unnecessary fields.
This keeps the design cleaner and matches how Video items are structured.

Patron eligibility is stored as a private boolean field.
By default, patrons are eligible, but this can be changed when the patron is created.
Borrowing checks a patron’s eligibility before allowing checkout, and there is a menu option to toggle this at runtime.

Assumptions and Limitations:

Files use UTF-8 encoding.

CSV parsing is simple: commas separate fields; quotes are not supported.

Item IDs start at 1000; patron IDs start at 1.

Each patron can borrow up to 3 items.

Invalid video ratings default to PG.

If input files are missing or invalid, the program exits with an error.

Bad lines in files print a warning and are skipped.

All data is stored in memory; nothing is saved after the program ends.

Compilation and Execution:

javac *.java
java Main patrons.txt items.txt

Sample files included:
patrons.txt
items.txt

These files contain example data for testing and demonstration.