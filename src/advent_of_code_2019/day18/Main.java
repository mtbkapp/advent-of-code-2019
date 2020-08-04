package advent_of_code_2019.day18;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;

public class Main {

  public static String readInput() throws Exception {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    Path p = Paths.get(loader.getResource("day_18.txt").toURI());
    byte[] bs = Files.readAllBytes(p);

    return new String(bs, "UTF-8"); 
  }

  public static void solvePart1() throws Exception {
    long start = System.nanoTime();
    int minDist = Maze.fromString(readInput()).minDistToAllKeysSingleStart();
    long nanos = System.nanoTime() - start;
    System.out.println("dist = " + minDist + " in " + (nanos / 100000.0) + " ms");
  }

  public static void solvePart2() throws Exception {
    Maze m = Maze.fromString(readInput());
    m.toPart2Maze();
    long start = System.nanoTime();
    int minDist = m.minDistFromAllStarts();
    long nanos = System.nanoTime() - start;
    System.out.println("dist = " + minDist + " in " + (nanos / 100000.0) + " ms");
  }

  public static void main(String[] args) throws Exception {
    System.out.println("start");
    Test.test();
    solvePart2();
    System.out.println("done");
  }
}
