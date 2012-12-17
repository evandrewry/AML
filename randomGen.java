    public static void randomGenMaze() {
        width = (int)(Math.random() * 20) + 5;
        height = (int)(Math.random() * 20) + 5;
        maze = new Cell[height][width];
        int targetRow = (int)(Math.random()*(height-1));
        int targetCol = (int)(Math.random()*(width-1));
        maze[targetRow][targetCol] = new Cell(3, targetRow, targetCol); // randomly generate a target cell
        int stepLength = (int)((Math.random()*width)/2 + (Math.random()*height)/2) + width/2 + height/2; // determine how many steps it'll iterate back from the target until a start cell is picked
        boolean pathGenerated = false;
        while (!pathGenerated) pathGenerated =  pathGen(maze[targetRow][targetCol], stepLength);
        // start point has been picked - for remaining null cells, randomly pick between a step or a hole
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                if (maze[row][col] == null) {
                    double prob = Math.random();
                    if (prob < .3) maze[row][col] = new Cell(1, row, col);
                    else maze[row][col] = new Cell(0, row, col);
                }
            }
        }
    }
    
    public static boolean pathGen(Cell c, int steps) {
        int cRow = c.getRow();
        int cCol = c.getCol();
        if (steps == 0) { // make this cell the starting one
            maze[cRow][cCol] = new Cell(2, cRow, cCol);
            return true;
        }
        LinkedList<String> dirs = new LinkedList<String>();
        dirs.add("up");
        dirs.add("down");
        dirs.add("left");
        dirs.add("right");
        if (cRow == 0) dirs.remove("up");
        if (cRow == (height-1)) dirs.remove("down");
        if (cCol == 0) dirs.remove("left");
        if (cCol == (width-1)) dirs.remove("right");
        if (dirs.size() == 0) return false;
        String randDir = dirs.get((int)(Math.random() * dirs.size())); // pick a random direction from the current cell
        if (randDir.equals("up")) {
            if (maze[cRow-1][cCol] == null) maze[cRow-1][cCol] = new Cell(1, cRow-1, cCol);            
            steps--;
            return pathGen(maze[cRow-1][cCol], steps);
        }
        else if (randDir.equals("down")) {
            if (maze[cRow+1][cCol] == null) maze[cRow+1][cCol] = new Cell(1, cRow+1, cCol);
            steps--;
            return pathGen(maze[cRow+1][cCol], steps);
        }
        else if (randDir.equals("left")) {
            if (maze[cRow][cCol-1] == null) maze[cRow][cCol-1] = new Cell(1, cRow, cCol-1);
            steps--;
            return pathGen(maze[cRow][cCol-1], steps);
        }
        else {
            if (maze[cRow][cCol+1] == null) maze[cRow][cCol+1] = new Cell(1, cRow, cCol+1);
            steps--;
            return pathGen(maze[cRow][cCol+1], steps);
        }
        
    }
