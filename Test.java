
public class Test
{
    public static void main(String [] args) throws Exception {
        AMLJava.buildMaze("random");
        AMLJava.move_U();
        AMLJava.move_L();
        AMLJava.move_D();
        AMLJava.move_R();
        AMLJava.revert();
        AMLJava.revert();
    } 
    
    public static void testList() {
        List x = new List(new Object[] {0, 1, 5, 7});
        List list = new List(new Object [] {new List(new Object[] {0, 1, 5, 7}), new List(new Object[] {0, 1, 5, 7})});
    }
}
