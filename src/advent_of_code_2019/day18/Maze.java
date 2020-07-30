package advent_of_code_2019.day18;


public class Maze {
  private Cell[][] cells;
  public final Vector start;
  public final int keyCount;

  private Maze(Cell[][] cells, Vector start, int keyCount) {
    this.cells = cells;
    this.start = start;
    this.keyCount = keyCount;
  }

  public Cell getCell(int x, int y) {
    if (x >= 0 && x < getWidth() && y >= 0 && y < getHeight()) {
      return cells[y][x];
    }

    return null;
  }

  public Cell getCell(Vector pos) {
    return getCell(pos.x, pos.y);
  }

  public int getHeight() {
    return cells.length;
  }

  public int getWidth() {
    return cells[0].length;
  }

  @Override
  public String toString() {
    StringBuffer buff = new StringBuffer();

    for(Cell[] row : cells) {
      for(Cell cell : row) {
        buff.append(cell.symbol);
      }
      buff.append("\n");
    }

    return buff.toString(); 
  }

  public static Maze fromString(String input) {
    String[] lines = input.split("\n");
    int height = lines.length;
    int width = lines[0].length();
    int keyCount = 0;
    Cell[][] cells = new Cell[height][width];
    Vector start = null;

    for(int y = 0; y < height; y++) {
      String line = lines[y];

      for(int x = 0; x < width; x++) {
        char c = line.charAt(x); 
        Cell cell = Cell.fromChar(c);
        cells[y][x] = cell;

        if (cell.type == Cell.CellType.START) {
          start = new Vector(x, y);
        } else if (cell.type == Cell.CellType.KEY) {
          keyCount++;
        }
      }
    }

    return new Maze(cells, start, keyCount); 
  }
}
