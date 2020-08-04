package advent_of_code_2019.day18;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public interface Node {
  public int keyCount();
  public List<Node> adjacent(Maze maze); 
  public KeySet getKeys();
}
