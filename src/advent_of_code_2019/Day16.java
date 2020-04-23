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

  private long multAdd(long[] a, long[] b) {
    long out = 0;
    for(int i = 0; i < a.length; i++) {
      out += (a[i] * b[i]);
    }

    return Math.abs(out) % 10;
  }

  private long[] phase(long[] input) {
    long[] output = new long[input.length];

    for(int i = 0; i < input.length; i++) {
      long[] repeatingSeq = (new RepeatingSeq(i)).take(input.length);
      output[i] = multAdd(repeatingSeq, input);
    }

    return output;
  } 

  public void go() {
    long[] s = phase(parseInput("12345678")); 
    for (int i = 0; i < s.length; i++) {
      System.out.println(s[i]);
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
