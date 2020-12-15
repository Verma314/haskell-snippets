* Passing a function to another function in java
```
interface Operation {
  int operate(int x);
}

class Solution {
  

    public static int ifEvenThenApplyFunction (Operation operation, int y ) {
        if ( y%2 == 0) {
            return operation.operate(y);
        } else {
            return y;
        }
    }

    public static void main() {
        System.out.println(ifEvenThenApplyFunction(x -> x + 1, 10));
    }
}
```