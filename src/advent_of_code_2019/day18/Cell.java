package advent_of_code_2019.day18;


public class Cell {
  public enum CellType {
    WALL,
    SPACE,
    KEY,
    DOOR,
    START
  }

  public final CellType type;
  public final char symbol;

  public Cell(CellType type, char symbol) {
    this.type = type;
    this.symbol = symbol; 
  }

  public static Cell fromChar(char c) {
    if (c == '#') {
      return new Cell(CellType.WALL, c);
    } else if (c == '.') {
      return new Cell(CellType.SPACE, c);
    } else if (c == '@') {
      return new Cell(CellType.START, c);
    } else if (Character.isUpperCase(c)) {
      return new Cell(CellType.DOOR, c);
    } else if (Character.isLowerCase(c)) {
      return new Cell(CellType.KEY, c);
    } else {
      throw new IllegalArgumentException("Invalid cell character: " + c);
    }
  }

  public boolean isDoor() {
    return type == CellType.DOOR;
  }

  public boolean isKey() {
    return type == CellType.KEY;
  }

  public boolean isSpace() {
    return type == CellType.SPACE || type == CellType.START;

  }
}
