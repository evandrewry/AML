
public class Test
{
    public static void main(String [] args) throws Exception {
        AMLJava.buildMaze();
        AMLJava.move_U();
        AMLJava.move_L();
        AMLJava.move_D();
        AMLJava.move_R();
        AMLJava.revert();
        AMLJava.revert();
    }        
}
