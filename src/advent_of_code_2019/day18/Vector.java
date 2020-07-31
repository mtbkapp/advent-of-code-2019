package advent_of_code_2019.day18;

import java.util.Objects;

public class Vector {
  public final int x;
  public final int y;

  public Vector(int x, int y) {
    this.x = x;
    this.y = y;
  }

  public Vector[] adjacent() {
    Vector[] adj = new Vector[4];

    adj[0] = new Vector(x - 1, y);
    adj[1] = new Vector(x + 1, y);
    adj[2] = new Vector(x, (y - 1));
    adj[3] = new Vector(x, (y + 1));

    return adj;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof Vector) {
      Vector rhs = (Vector) obj;
      return x == rhs.x && y == rhs.y;
    }

    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(x, y);
  }

  @Override
  public String toString() {
    return "Vector(" + x + ", " + y + ")";
  }
}
