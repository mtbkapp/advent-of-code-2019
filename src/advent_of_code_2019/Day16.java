package advent_of_code_2019;


public class Day16 {

  class RepeatingSeq {
    private final long[] base = new long[]{0, 1, 0, -1};
    private int repeatReset;
    private int repeats;
    private int baseIdx = 0;

    RepeatingSeq(int repeats) {
      this.repeatReset = repeats;
      this.repeats = repeats;
    }

    long next() {
      repeats--;
      if (repeats == 0) {
        repeats = repeatReset;
        baseIdx = (baseIdx + 1) % base.length;
      }

      return base[baseIdx];
    }

    long[] take(int n) {
      long[] out = new long[n];

      for(int i = 0; i < n; i++) {
        out[i] = next();
      }

      return out;
    }
  }

  private long[] phase(long[] input) {
    long[] output = new long[input.length];

    for(int i = 0; i < input.length; i++) {
      long[] repeatingSeq = (new RepeatingSeq(i)).take(input.length);
    }

    return output;
  } 

  public void go() {
    RepeatingSeq s = new RepeatingSeq(2);
    for (int i = 0; i < 10; i++) {
      System.out.println(s.next());
    }
  }

  static long[] parseInput(String input) {
    long[] out = new long[input.length()];
    int i = 0;

    for(String digit : input.split("")) {
      out[i] = Long.valueOf(digit);
      i++;
    }

    return out;
  }

  public static void main(String[] args) {
    (new Day16()).go();
  }
}
