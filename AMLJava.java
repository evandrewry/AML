import java.util.*;
import java.io.*;
import javax.swing.*;
import java.awt.*;

/*
 * Standard Library of Java code for amL
 * Programming Languages and Translators, Fall 2012
 * 
 * Sriramkumar Balasubramanian (sb3457)
 * Evan Drewry (ewd2106)
 * Timothy Giel (tkg2104)
 * Nikhil Helferty (nh2407)
 * 
 * Includes functions for the bot to move around the maze, or
 * obtain information about its surrounding environment.
 * 
 * Includes a rudimentary Swing GUI for the user to see
 * what the bot does when the program is run.
 * 
 * Makes use of the custom Cell object to represent a given cell of the maze.
 * 
 */
public class AMLJava extends JFrame
{
    static int width; // width of the maze
    static int height; // height of the maze
    static Cell current; // the current cell the bot is at
    static Cell [][] maze; // 2D representation of the maze - maze[row][col] is Cell at row row, column col, with top left being 0, 0
    static Stack<Cell> moves; // is a stack of Cells - consecutive moves that have been done, not counting "reverted" moves - used to backtrack in revert()
    
    // build the representation of the maze from the text file
    public static void buildMaze() {
        File mazeFile  = new File("maze.txt");
        try {
            Scanner scan = new Scanner(mazeFile);
            width = scan.nextInt();
            height = scan.nextInt();
            maze = new Cell[height][width];
            for (int row = 0; row < height; row++) {
                for (int col = 0; col < width; col++) {
                    int temp = scan.nextInt();
                    maze[row][col] = new Cell(temp, row, col);
                    if (temp == 2) current = maze[row][col];
                }
            }
            scan.close();
        }
        catch (FileNotFoundException e) {
            System.out.println("File Not Found");
            return;
        }
        moves = new Stack<Cell>();
        new AMLJava(); // initiliase the Swing GUI
    }
    
    // creates the Swing GUI
    public AMLJava() {
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setTitle("Maze");
        JPanel mazePanel = new JPanel(new GridLayout(height, width)); // height bounds # of rows, width bounds # of columns
        for(int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                mazePanel.add(maze[row][col]); // add the cell to the maze
            }
        }
        add(mazePanel, BorderLayout.CENTER);
        pack();
        setVisible(true);
    }
    
    // moves the bot up from its current position
    // if successful returns true, otherwise false
    public static boolean move_U() {
        if (hasTop()) {
            move(maze[current.getRow()-1][current.getCol()]);
            return true;
        }
        else return false;
    }
    
    public static boolean move_D(){
        if (hasBottom()) {
            move(maze[current.getRow()+1][current.getCol()]);
            return true;            
        }
        else return false;
    }
    
    public static boolean move_L() {
        if (hasLeft()) {
            move(maze[current.getRow()][current.getCol()-1]);
            return true;
        }        
        else return false;
    }
    
    public static boolean move_R() {
        if (hasRight()) {
            move(maze[current.getRow()][current.getCol()+1]);
            return true;
        }
        else return false;   
    }
    
    // private move function eliminating duplicate code
    // moves the bot from "current" cell to next cell in parameter, updates the GUI accordingly
    private static void move(Cell next) {
        moves.push(current);
        try {
            Thread.sleep(500);
        }
        catch (InterruptedException e) { }
        current.setText("");
        if (current.isTarget()) current.setText("TARGET");
        current = next;
        current.visited();
        current.setText("BOT");        
    }
    
    // returns true if there is a cell the bot can go on above it
    // false otherwise
    public static boolean hasTop() {
        if (current.getRow() > 0) { // if not at top
            if (maze[current.getRow()-1][current.getCol()].getValue() != 0) return true; // if not a "hole"
        }
        return false;
    }
    
    public static boolean hasBottom() {
        if (current.getRow() < (height-1)) {
            if (maze[current.getRow()+1][current.getCol()].getValue() != 0) return true;
        }
        return false;
    }
            
    public static boolean hasLeft() {
        if (current.getCol() > 0) {
            if (maze[current.getRow()][current.getCol()-1].getValue() != 0) return true;
        }
        return false;
    }
    
    public static boolean hasRight() {
        if (current.getCol() < (width-1)) {
            if (maze[current.getRow()][current.getCol()+1].getValue() != 0) return true;
        }
        return false;
    }
    
    // returns whether or not the cell at row, col has been visited
    public static boolean visit(int row, int col) {
        return maze[row][col].getVisited();
    }
    
    // overloaded version of visit that instead accepts a single integer (cell ID)
    // cell ID is calculated as follows = (width of maze) * (cell row) + (cell column)
    public static boolean visit(int id) {
        return visit(id/width, id%width);
    }
    
    // "reverts" the previous move if possible (backtracks), returns true
    // if no moves committed returns false
    public static boolean revert() {
        if (moves.empty()) return false; // no moves executed!
        else {
            try {
                Thread.sleep(500);
            }
            catch (InterruptedException e) { }
            if (current.isTarget()) current.setText("TARGET");
            else current.setText("");
            current = moves.pop();
            current.setText("BOT");
            return true;
        }        
    }
    
    
    
}
