package advent_of_code_2019;


public class Day17 {

  static String lrss(String in) {
    int n = in.length();
    int[][] table = new int[n + 1][n + 1];
    int resLen = 0, index = 0;
    String res = "";

    for (int i = 1; i <= n; i++) {
      for(int j = i + 1; j <= n; j++) {
        if(in.charAt(i - 1) == in.charAt(j - 1) &&
            table[i - 1][j - 1] < (j - i)) {
          table[i][j] = table[i - 1][j - 1] + 1;
          if(table[i][j] > resLen) {
            resLen = table[i][j];
            index = Math.max(i, index);
          }
        } 
      }
    }

    prnTable(table);

    if (resLen > 0) {
      for(int i = index - resLen + 1; i <= index; i++) {
        res += in.charAt(i - 1);
      }
    }

    return res;
  }

  static void prnTable(int[][] table) {
    for(int[] row : table) {
      for(int col : row) {
        System.out.print(col + " ");
      }
      System.out.println();
    }
  }


  public static void main(String[] args) {
    System.out.println(lrss("aaabbbaaaaaabbbbbb"));
  }
}
