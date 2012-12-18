import java.util.*;

public class dfs{
public static void main(String[] args){
AMLJava.buildMaze("random");
DFS();
}


public static void DFS(){
Cell node = AMLJava.current;
if (node.isTarget())
{
return;
}
if (myvisited(node))
{
DFS();
}
else
{
if (node.isSource())
{
return;
}
AMLJava.revert();
DFS();
}
}


public static Boolean myvisited(Cell node){
if (AMLJava.hasLeft() && !AMLJava.left().getVisited())
{
AMLJava.move_L();
}
else
{
if (AMLJava.hasTop() && !AMLJava.up().getVisited())
{
AMLJava.move_U();
}
else
{
if (AMLJava.hasRight() && !AMLJava.right().getVisited())
{
AMLJava.move_R();
}
else
{
if (AMLJava.hasBottom() && !AMLJava.down().getVisited())
{
AMLJava.move_D();
}
else
{
return false;
}
}
}
}
return true;
}
}