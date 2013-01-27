/*
 * Copyright (C) 2012 Lalit Pant <pant.lalit@gmail.com>
 *
 * The contents of this file are subject to the GNU General Public License
 * Version 3 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.gnu.org/copyleft/gpl.html
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 */

package net.kogics.kojo
package xscala

// Do not format source. It messes up help code formatting.

object Help {

  implicit def elem2str(e: xml.Elem) = e.toString
  val CommonContent = Map[String, String](
    "repeat" -> 
    <div>
      <strong>repeat</strong>(n){{ }} - Repeats a block of commands (within braces) n number of times.<br/>
      <br/>
      <em>Example:</em> <br/><br/>
      <pre>
        clear()
        // make a square with the help of the repeat command
        repeat (4) {{
          forward(100)
          right()
        }}
      </pre>
    </div>
    ,
    "repeati" -> "repeati(n) {i => } - Repeats the commands within braces n number of times. The current repeat index is available within the braces.",
    "repeatWhile" -> "repeatWhile(cond) {} - Repeats the commands within braces while the given condition is true.",
    "repeatUntil" -> "repeatUntil(cond) {} - Repeats the commands within braces until the given condition is true.",
    "zoom" -> 
    """zoom(factor) - Zooms in by the given factor, leaving the center point unchanged.<br/>
       <br/>
       zoom(factor, cx, cy) - Zooms in by the given factor, and positions (cx, cy) at the center of the turtle canvas.
""",
    "gridOn" -> "gridOn() - Shows a grid on the turtle canvas.",
    "gridOff" -> "gridOff() - Hides the grid on the turtle canvas.",
    "axesOn" -> "axesOn() - Shows the X and Y axes on the turtle canvas.",
    "axesOff" -> "axesOff() - Hides the X and Y axes on the turtle canvas.",
    "showScriptInOutput" -> "showScriptInOutput() - Enables the display of scripts in the output window when they run.",
    "hideScriptInOutput" -> "hideScriptInOutput() - Stops the display of scripts in the output window.",
    "showVerboseOutput" -> "showVerboseOutput() - Enables the display of output from the Scala interpreter. By default, output from the interpreter is shown only for single line scripts.",
    "hideVerboseOutput" -> "hideVerboseOutput() - Stops the display of output from the Scala interpreter.",
    "retainSingleLineCode" -> "retainSingleLineCode() - A Kojo 1.0 command. Does nothing now.",
    "clearSingleLineCode" -> "clearSingleLineCode() - A Kojo 1.0 command. Does nothing now.",
    "version" -> "version - Displays the version of Scala being used.",
    "println" -> "println(obj) - Displays the given object as a string in the output window, with a newline at the end.",
    "print" -> "print(obj) - Displays the given object as a string in the output window, without a newline at the end.",
    "readln" -> "readln(promptString) - Displays the given prompt in the output window and reads a line that the user enters.",
    "readInt" -> "readInt(promptString) - Displays the given prompt in the output window and reads an Integer value that the user enters.",
    "readDouble" -> "readDouble(promptString) - Displays the given prompt in the output window and reads a Double-precision Real value that the user enters.",
    "random" -> "random(upperBound) - Returns a random Integer between 0 (inclusive) and upperBound (exclusive).",
    "randomDouble" -> "randomDouble(upperBound) - Returns a random Double-precision Real between 0 (inclusive) and upperBound (exclusive).",
    "inspect" -> "inspect(obj) - Opens up a window showing the internal fields of the given object",
    "playMusic" -> "playMusic(score) - Plays the specified melody, rhythm, or score.",
    "playMusicUntilDone" -> "playMusicUntilDone(score) - Plays the specified melody, rhythm, or score, and waits till the music finishes.",
    "playMusicLoop" -> "playMusicLoop(score) - Plays the specified melody, rhythm, or score in the background - in a loop.",
    "textExtent" -> "textExtent(text, fontSize) - Determines the size/extent of the given text fragment for the given font size.",
    "runInBackground" -> "runInBackground(command) - Runs the given code in the background, concurrently with other code that follows right after this command.",
    "playMp3" -> "playMp3(fileName) - Plays the specified MP3 file.",
    "playMp3Loop" -> "playMp3Loop(fileName) - Plays the specified MP3 file in the background.",
    "ColorHSB" -> "ColorHSB(h, s, b) - Creates a color with the given Hue (0-360), Saturation (0-100), and Brighness (0-100) values.",
    "Color" -> "Color(r, g, b, opac) - Creates a color with the given red, green, blue, and opacity (optional) values.",
    "ColorG" -> "ColorG(x1, y1, color1, x2, y2, color2, cyclic) - Creates a color gradient for filling shapes. The cyclic value is optional.",
    "setBackground" -> "setBackground(color) - Sets the canvas background to the specified color. You can use predefined colors for setting the background, or you can create your own colors using the Color, ColorHSB, and ColorG functions.",
    "setBackgroundH" -> "setBackgroundH(color1, color2) - Sets the canvas background to a horizontal color gradient defined by the two specified colors.",
    "setBackgroundV" -> "setBackgroundV(color1, color2) - Sets the canvas background to a vertical color gradient defined by the two specified colors."
  )

  val TwContent = Map[String, String](
    "forward" -> 
    <div>
      <strong>forward</strong>(numSteps) - Moves the turtle forward by the given number of steps. <br/>
      <br/>
      <em>Example:</em> <br/><br/>
      <pre>
        clear()
        // move forward by 100 steps
        forward(100) 
          
        // move forward by 200 steps
        forward(200)
      </pre>
    </div>
    ,
    
    "back" -> 
    <div>
      <strong>back</strong>(numSteps) - Moves the turtle back by the given number of steps. <br/>
      <br/>
      <em>Example:</em> <br/><br/>
      <pre>
        clear()
        // move back by 100 steps
        back(100)
                        
        // move back by 200 steps
        back(200)
      </pre>
    </div>
    ,
    "home" -> 
    <div>
      <strong>home</strong>() - Moves the turtle to its original location, and makes it point north. <br/>
      <br/>
      <em>Example:</em> <br/><br/>
      <pre>
        clear()
        // move the turtle out
        forward(100)
        right()
        forward(50)
          
        // now take it back home
        home()
      </pre>
    </div>
    ,
    "setPosition" -> 
    <div>
      <strong>setPosition</strong>() - (x, y) - Sends the turtle to the point (x, y) without drawing a line. The turtle's heading is not changed. <br/>
      <br/>
      <em>Examples:</em> <br/><br/>
      <pre>
        setPosition(100, 50)
                  
        setPosition(80, 150)
      </pre>
    </div>
    ,
    "position" -> 
    <div>
      <strong>position</strong> - Tells you the turtle's current position. <br/>
      <br/>
      <em>Example:</em> <br/><br/>
      <pre>
        clear()
        // move the turtle out
        forward(100)
        right()
        forward(50)
          
        // now report its position
        print(position) // Point(50.00, 100.00)
      </pre>
    </div>
    ,
    "style" -> "style - Tells you the turtle's current style. See the help for saveStyle() for more information on styles.",
    "moveTo" -> "moveTo(x, y) - Turns the turtle towards (x, y) and moves the turtle to that point. ",
    "turn" -> 
    <div>
      <strong>turn</strong>(angle) - Turns the turtle through the specified angle.<br/>
      Positive angles are in the anti-clockwise direction. Negative angles are in the clockwise direction. <br/>
      <br/>
      <em>Note: </em>It's easier to use <strong>left</strong>(angle) or <strong>right</strong>(angle) to turn the turtle.
    </div>
    ,
    "right" -> 
    <div>
      <strong>right</strong>() - Turns the turtle 90 degrees right (clockwise). <br/>
      <strong>right</strong>(angle) - Turns the turtle right (clockwise) through the given angle in degrees.<br/>
      <br/>
      <em>Examples:</em> <br/>
      <br/>
      <pre>
        // turn right by 90 degrees
        right()
                
        // turn right by 30 degrees
        right(30)
      </pre>
    </div>
    ,
    "left" -> 
    <div>
      <strong>left</strong>() - Turns the turtle 90 degrees left (anti-clockwise). <br/>
      <strong>left</strong>(angle) - Turns the turtle left (anti-clockwise) through the given angle in degrees.<br/>
      <br/>
      <em>Examples:</em> <br/>
      <br/>
      <pre>
        // turn left by 90 degrees
        left()
                
        // turn left by 30 degrees
        left(30)
      </pre>
    </div>
    ,
    "towards" -> "towards(x, y) - Turns the turtle towards the point (x, y).",
    "setHeading" -> "setHeading(angle) - Sets the turtle's heading to angle (0 is towards the right side of the screen ('east'), 90 is up ('north')).",
    "heading" -> "heading - Queries the turtle's heading (0 is towards the right side of the screen ('east'), 90 is up ('north')).",
    "penDown" -> 
    <div>
      <strong>penDown</strong>() - Pushes the turtle's pen down, and makes it draw lines as it moves. <br/>
      The turtle's pen is down by default. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        clear()
        // pull the turtle's pen up
        penUp()
        // the turtle moves forward without drawing a line
        forward(100) 
                    
        // push the turtle's pen down
        penDown()
        // now the turtle draws a line as it moves forward
        forward(100) 
      </pre>
    </div>
    ,
    "penUp" -> 
    <div>
      <strong>penUp</strong>() - Pulls the turtle's pen up, and prevents it from drawing lines as it moves. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        clear()
        // pull the turtle's pen up
        penUp()
        // the turtle moves forward without drawing a line
        forward(100) 
                    
        // push the turtle's pen down
        penDown()
        // now the turtle draws a line as it moves forward
        forward(100) 
      </pre>
    </div>
    ,
    "setPenColor" -> 
    <div>
      <strong>setPenColor</strong>(color) - Specifies the color of the pen that the turtle draws with. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        clear()
        setPenColor(blue)
        // makes a blue line
        forward(100)
                    
        setPenColor(green)
        // makes a green line
        forward(100)
      </pre>
    </div>
    ,
    "setFillColor" -> 
    <div>
      <strong>setFillColor</strong>(color) - Specifies the fill color of the figures drawn by the turtle. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        clear()
        setFillColor(blue)
        // make a circle filled with blue
        circle(50)
                    
        setFillColor(green)
        // make a circle filled with green
        circle(50)
      </pre>
    </div>
    ,
    "setPenThickness" -> 
    <div>
      <strong>setPenThickness</strong>(thickness) - Specifies the width of the pen that the turtle draws with. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        clear()
        setPenThickness(10)
        // make a line that is 10 units thick
        forward(100)
                    
        setPenThickness(15)
        // make a line that is 15 units thick
        forward(100)
      </pre>
    </div>
    ,
    "setPenFontSize" -> 
    <div>
      <strong>setPenFontSize</strong>(n) - Specifies the font size of the pen that the turtle writes with. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        clear()
        setPenFontSize(15)
        // write with a font size of 15
        write("Hi There")
        forward(50)            
        setPenFontSize(20)
        // write with a font size of 20
        write("Hi There")
      </pre>
    </div>
    ,
    "savePosHe" -> 
    <div>
      <strong>savePosHe</strong>() - Saves the turtle's current position and heading, so that they can 
      easily be restored later with a <tt>restorePosHe()</tt>.<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        clear()
        // save the turtle's position and heading
        savePosHe()
                    
        // move wherever
        forward(100)
        right(45)
        forward(60)
                    
        // now restore the saved position and heading, 
        // so that the turtles gets back to 
        // exactly where it started out from 
        restorePosHe()
      </pre>
    </div>
    ,
    "restorePosHe" -> 
    <div>
      <strong>restorePosHe</strong>() - Restores the turtle's current position and heading 
      based on an earlier <tt>savePosHe()</tt>.<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        clear()
        // save the turtle's position and heading
        savePosHe()
                    
        // move wherever
        forward(100)
        right(45)
        forward(60)
                    
        // now restore the saved position and heading, 
        // so that the turtles gets back to 
        // exactly where it started out from 
        restorePosHe()
      </pre>
    </div>
    ,
    "saveStyle" -> 
    <div>
      <strong>saveStyle</strong>() - Saves the turtle's current style, so that it can 
      easily be restored later with <tt>restoreStyle()</tt> .<br/>
      <p>
        The turtle's style includes:
        <ul>
          <li>Pen Color</li>
          <li>Pen Thickness</li>
          <li>Fill color</li>
          <li>Pen Font Size</li>
        </ul>
      </p>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def tick(n: Int) {{
            // save current style, position and heading
            saveStyle()
            savePosHe()
            setPenColor(gray)
            right()
            forward(n)
            back(n * 2)
            restorePosHe()
            restoreStyle()
            // restore caller's style, position and heading
        }}

        clear()
        setPenColor(green)
        right()
        // green line
        forward(100)
        // grey tick
        tick(10)
        // green line
        forward(100)
      </pre>
    </div>
    ,
    "restoreStyle" -> 
    <div>
      <strong>restoreStyle</strong>() - Restores the turtle's style
      based on an earlier <tt>saveStyle()</tt>.
      <br/>
      <p>
        The turtle's style includes:
        <ul>
          <li>Pen Color</li>
          <li>Pen Thickness</li>
          <li>Fill color</li>
          <li>Pen Font Size</li>
        </ul>
      </p>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def tick(n: Int) {{
            // save current style, position and heading
            saveStyle()
            savePosHe()
            setPenColor(gray)
            right()
            forward(n)
            back(n * 2)
            restorePosHe()
            restoreStyle()
            // restore caller's style, position and heading
        }}

        clear()
        setPenColor(green)
        right()
        // green line
        forward(100)
        // grey tick
        tick(10)
        // green line
        forward(100)
      </pre>
    </div>
    ,
    "beamsOn" -> "beamsOn() - Shows crossbeams centered on the turtle - to help with thinking about the turtle's heading/orientation.",
    "beamsOff" -> "beamsOff() - Hides the turtle crossbeams that are turned on by beamsOn().",
    "invisible" -> "invisible() - Hides the turtle.",
    "visible" -> "visible() - Makes the hidden turtle visible again.",
    "write" -> "write(obj) - Makes the turtle write the specified object as a string at its current location.",
    "setAnimationDelay" -> 
    <div>
      <strong>setAnimationDelay</strong>(delay) - Sets the turtle's speed. The specified delay 
      is the amount of time (in milliseconds) taken by the turtle to move through a distance of one hundred steps.<br/>
      The default delay is 1000 milliseconds (or 1 second).<br/>
      <br/>
      <em>Examples:</em> <br/>
      <br/>
      <pre>
        // default animation delay
        // drawing the line takes 1 second
        forward(100)
                    
        setAnimationDelay(500)
        // drawing the line takes 1/2 seconds
        forward(100)
                    
        setAnimationDelay(100)
        // drawing the line takes 1/10 seconds
        forward(100)
      </pre>
    </div>
    ,
    "animationDelay" -> "animationDelay - Queries the turtle's delay setting.",
    "clear" -> "clear() - Clears the turtle canvas, and brings the turtle to the center of the canvas.",
    "cleari" -> "cleari() - Clears the turtle canvas and makes the turtle invisible.",
    "wipe" -> "wipe() - Wipes the turtle canvas by earsing all pictures. Meant to be used during an animation.",
    "clearOutput" -> "clearOutput() - Clears the output window.",
    "clearWithUL" -> "clearWithUL(unit) - Clears the turtle canvas, sets the given unit length (Pixel, Cm, or Inch), and brings the turtle to the center of the canvas.",
    "arc" ->
    <div>
        <strong>arc</strong>(radius, angle) - Gets the turtle to make an arc with the given 
        radius and angle.<br/>
        Positive angles make the turtle go left (ant-clockwise). Negative angles make the turtle go right (clockwise) <br/>
        <br/>
        <em>Examples:</em> <br/>
        <br/>
        <pre>
            // a simple arc
            clear()    
            arc(100, 45)

            // a pattern of arcs
            clear()   
            right(135)
            repeat (5) {{
              arc(50, 90)
              arc(50, -90)
            }}
        </pre>
    </div>
    ,
    "circle" ->
    <div>
        <strong>circle</strong>(radius) - Gets the turtle to make a circle with the given 
        radius. <br/>
        A circle(50) command is equivalent to an arc(50, 360) command.<br/>
        <br/>
        <em>Example:</em> <br/>
        <br/>
        <pre>
            clear()    
            circle(50)
        </pre>
    </div>
    ,
    "==" -> 
    <div>
        <strong>a == b</strong> - Evaluates to true if a and b are equal, false otherwise.<br/>
        <br/>
        <em>Example:</em> <br/>
        <br/>
        <pre>
            // Change a and b to play with this
            val a = 10 
            val b = 20
            if (a == b) {{
              println("a is equal to be")
            }}
            else {{
              println("a and b are different")  
            }}
        </pre>
        <em>Example2:</em> <br/>
        <br/>
        <pre>
            def isZero(n: Int) = 
              n == 0
        </pre>
    </div>
    ,
    "!=" -> 
        <div>
        <strong>a != b</strong> - Evaluates to true if a and b are not equal, false otherwise.<br/>
        <br/>
        <em>Example:</em> <br/>
        <br/>
        <pre>
            // Change a and b to play with this
            val a = 10 
            val b = 20
            if (a != b) {{
              println("a and b are different")  
            }}
            else {{
              println("a is equal to be")
            }}
        </pre>
        <em>Example2:</em> <br/>
        <br/>
        <pre>
            def isNotZero(n: Int) = 
              n != 0
        </pre>
    </div>
    ,
    ">" ->     
    <div>
        <strong>a > b</strong> - Evaluates to true if a is greater than b, false otherwise.<br/>
        <br/>
        <em>Example:</em> <br/>
        <br/>
        <pre>
            // Change a and b to play with this
            val a = 10
            val b = 20
            if (a > b) {{
              println("a is bigger")
            }}
            else {{
              println("b is bigger")  
            }}
        </pre>
        <em>Example2:</em> <br/>
        <br/>
        <pre>
            def max(a: Int, b: Int) = if (a > b) a else b
            max(5, 10) // 10
        </pre>
    </div>
    ,
    "<" -> 
    <div>
        <strong>a &lt; b</strong> - Evaluates to true if a is less than b, false otherwise.<br/>
        <br/>
        <em>Example:</em> <br/>
        <br/>
        <pre>
            // Change a and b to play with this
            val a = 10
            val b = 20
            if (a &lt; b) {{
              println("a is smaller")
            }}
            else {{
              println("b is smaller")  
            }}
        </pre>
        <em>Example2:</em> <br/>
        <br/>
        <pre>
            def min(a: Int, b: Int) = if (a &lt; b) a else b
            min(5, 10) // 5
        </pre>
    </div>
    ,
    ">=" -> 
     <div>
        <strong>a >= b</strong> - Evaluates to true if a is greater than or equal to b, false otherwise.<br/>
        <br/>
        <em>Example:</em> <br/>
        <br/>
        <pre>
            // Change a and b to play with this
            val a = 10
            val b = 20
            if (a >= b) {{
              println("a is bigger than or equal to b")
            }}
            else {{
              println("a is smaller than b")  
            }}
        </pre>
        <em>Example2:</em> <br/>
        <br/>
        <pre>
            def max(a: Int, b: Int) = if (a >= b) a else b
            max(5, 10) // 10
        </pre>
    </div>
    ,
    "<=" -> 
    <div>
        <strong>a &lt;= b</strong> - Evaluates to true if a is less than or equal b, false otherwise.<br/>
        <br/>
        <em>Example:</em> <br/>
        <br/>
        <pre>
            // Change a and b to play with this
            val a = 10
            val b = 20
            if (a &lt;= b) {{
              println("a is smaller than or equal to b")
            }}
            else {{
              println("a is bigger")  
            }}
        </pre>
        <em>Example2:</em> <br/>
        <br/>
        <pre>
            def min(a: Int, b: Int) = if (a &lt;= b) a else b
            min(5, 10) // 5
        </pre>
    </div>
    ,
    "for" ->
    <div>
      Usage #1 [with commands]:<br/>
      <strong>for</strong>(i &lt;- 1 to n) {{ commands }} - Repeats a block of commands (within braces) n number of times,
       making the repeat counter available within the block defined by the braces.<br/>
      <br/>
      Usage #2 [with an expression]:<br/>
      <strong>for</strong>(i &lt;- 1 to n) yield {{ expression }} - Processes the elements in a generator/collection, 
      yielding a new generator/collection.<br/>
      <br/>
      <em>Example:</em> <br/><br/>
      <pre>
          clear()
          setAnimationDelay(100)
          for (i &lt;- 1 to 4) {{
            val radius = 30 + i*10
            circle(radius)
          }}
      </pre>
      <em>Example 2:</em> <br/><br/>
      <pre>
          for (i &lt;- 1 to 4) yield (2 * i)
      </pre>
    </div>
    ,
    "def" ->
    <div>
        <strong>def</strong> - Gives a name to a block of commands (within braces) or an expression. This lets you define a 
        new command or function.<br/>
        <br/>
        <em>Examples:</em> <br/>
        <br/>
        <pre>
            // A new command named square
            // Takes one input
            def square(side: Int) {{
                repeat(4) {{
                    forward(side)
                    right()
                }}
            }}
            clear()
            // two different calls to square command
            square(100)
            square(200)


            // A new function named sum
            // Takes two inputs, and returns a result
            def sum(n1: Int, n2: Int) = 
                n1 + n2
            clearOutput()
            // a call to the sum function within a print command
            print(sum(3, 5))
            // another call to the sum function
            print(sum(20, 7))
        </pre>
    </div>
    ,
    "if" ->
    <div>
        <strong>if</strong> or <strong>if-else</strong> - Let's you choose the instruction to execute 
        based on a condition. The instruction can be a command, in which case if-else works as a command.
        Or the instruction can be an expression, in which case if-else works as an expression.<br/>
        <br/>
        <em>Examples:</em> <br/>
        <br/>
        <pre>
            clear()    
            val size = 50 
            // conditionally run a command
            // the else part is optional
            if (size > 100) {{
                setFillColor(blue)
            }}
            else {{
                setFillColor(green)
            }}
            circle(size)


            val n = 100
            // conditionally evaluate an expression
            val big = if (n > 50) true else false
            clearOutput()
            println(big)
        </pre>
    </div>
    ,
    "val" ->
    <div>
        <strong>val</strong> - Gives a name to an expression, letting you create a named value. 
        This makes your programs easier to modify and easier to understand.<br/>
        <br/>
        <em>Example:</em> <br/>
        <br/>
        <pre>
            clear()    
            val size = 50 
            circle(size)
            repeat (4) {{
                forward(size)
                right()
            }}
        </pre>
    </div>
    ,
    "pict" -> "pict { t => } is obsolete. Use the PictureT (preferred) or Picture function instead.",
    "Picture" -> 
    <div>
      <strong>Picture</strong>{{ drawingCode }} - Makes a picture out of the given turtle drawing code. <br/>
      The picture needs to be drawn for it to become visible in the turtle canvas. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        // create a function for making a picture with a circle in it
        def p = Picture {{
          circle(50)
        }}

        clear()
        invisible()
        // draw the picture
        draw(p)
      </pre>
    </div>
    ,
    "PictureT" -> 
    <div>
      <strong>PictureT</strong>{{ t => drawingCode }} - Makes a picture out of the given turtle drawing code, 
      which needs to draw using the supplied turtle <tt>t</tt>.<br/>
      The picture needs to be drawn for it to become visible in the turtle canvas. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        // create a function for making a picture with a circle in it
        def p = PictureT {{ t =>
          import t._
          circle(50)
        }}

        clear()
        invisible()
        // draw the picture
        draw(p)
      </pre>
    </div>
    ,
    "PicShape.hline" -> 
    <div>
      <strong>PicShape.hline</strong>(length) - Creates a picture of a horizontal line with the given length.<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        cleari()
        draw(PicShape.hline(50))
      </pre>
    </div>,
    "PicShape.vline" -> 
    <div>
      <strong>PicShape.vline</strong>(length) - Creates a picture of a vertical line with the given length.<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        cleari()
        draw(PicShape.vline(50))
      </pre>
    </div>,
    "PicShape.rect" -> 
    <div>
      <strong>PicShape.rect</strong>(height, width) - Creates a picture of a rectangle with the given height and width.<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        cleari()
        draw(PicShape.rect(50, 100))
      </pre>
    </div>,
    "PicShape.circle" -> 
    <div>
      <strong>PicShape.circle</strong>(radius) - Creates a picture of a circle with the given radius.<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        cleari()
        draw(PicShape.circle(50))
      </pre>
    </div>,
    "PicShape.text" -> 
    <div>
      <strong>PicShape.text</strong>(content, size) - Creates a picture of the given text with the given font-size.<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        cleari()
        draw(PicShape.text("Hi There", 18))
      </pre>
    </div>,
    "draw" -> 
    <div>
      <strong>draw</strong>(picture[s]) - Draws the given (one or more) picture(s).<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        cleari()
        draw(PicShape.hline(50), PicShape.vline(50)) 
      </pre>
    </div>,
    "HPics" -> 
    <div>
      <strong>HPics</strong>(pictures) <br/>
      Creates a horizontal row of the supplied pictures. Is equivalent to <strong>picRow</strong>(pictures)<br/>
      HPics is a container for pictures that lays out its child pictures horizontally. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = HPics(
            p,
            p,
            p
        )
        draw(pic)
      </pre>
    </div>
    ,
    "picRow" -> 
    <div>
      <strong>picRow</strong>(pictures) <br/>
      Creates a horizontal row of the supplied pictures. Is equivalent to <strong>HPics</strong>(pictures)<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = picRow(
            p,
            p,
            p
        )
        draw(pic)
      </pre>
    </div>
    ,
    "VPics" -> 
    <div>
      <strong>VPics</strong>(pictures) <br/>
      Creates a vertical column of the supplied pictures. Is equivalent to <strong>picCol</strong>(pictures)<br/>
      VPics is a container for pictures that lays out its child pictures vertically. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = VPics(
            p,
            p,
            p
        )
        draw(pic)
      </pre>
    </div>
    ,
    "picCol" -> 
    <div>
      <strong>picCol</strong>(pictures) <br/>
      Creates a vertical column of the supplied pictures. Is equivalent to <strong>VPics</strong>(pictures)<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = picCol(
            p,
            p,
            p
        )
        draw(pic)
      </pre>
    </div>
    ,
    "GPics" -> 
    <div>
      <strong>GPics</strong>(pictures) <br/>
      Creates a stack of the supplied pictures. Is equivalent to <strong>picStack</strong>(pictures)<br/>
      GPics is a container for pictures that lays out its child pictures one on top of the other. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = GPics(
            p,
            rot(30) -> p,
            rot(60) -> p
        )
        draw(pic)
      </pre>
    </div>
    ,
    "picStack" -> 
    <div>
      <strong>picStack</strong>(pictures) <br/>
      Creates a stack of the supplied pictures. Is equivalent to <strong>GPics</strong>(pictures)<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = picStack(
            p,
            rot(30) -> p,
            rot(60) -> p
        )
        draw(pic)
      </pre>
    </div>
    ,
    "rot" -> 
    <div>
      <strong>rot</strong>(angle) -> picture <br/>
      Rotates the given picture by the given angle. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = rot(30) -> p
        draw(pic)
      </pre>
    </div>
    ,
    "trans" -> 
    <div>
      <strong>trans</strong>(x, y) -> picture <br/>
      Translates the given picture by the given x and y values. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = trans(10, 5) -> p
        draw(pic)
      </pre>
    </div>,
    "offset" -> 
    <div>
      <strong>offset</strong>(x, y) -> picture <br/>
      Offsets the given picture by the given x and y values, with respect to the
      global (canvas) coordinate system. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        axesOn()
        val pic = rot(60) * offset(100, 0) -> p
        draw(pic)
      </pre>
    </div>,
    "scale" -> 
    <div>
      <strong>scale</strong>(factor) -> picture <br/>
      <strong>scale</strong>(xf, yf) -> picture <br/>
      Scales the given picture by the given scaling factor(s). <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = scale(2) -> p
        draw(pic)
      </pre>
    </div>,
    "fillColor" -> 
    <div>
      <strong>fillColor</strong>(color) -> picture <br/>
      Fills the given picture with the given color. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = fillColor(green) -> p
        draw(pic)
      </pre>
    </div>,
    "penColor" -> 
    <div>
      <strong>penColor</strong>(color) -> picture <br/>
      Sets the pen color for the given picture to the given color. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = penColor(blue) -> p
        draw(pic)
      </pre>
    </div>,
    "penWidth" -> 
    <div>
      <strong>penWidth</strong>(thickness) -> picture <br/>
      Sets the pen width for the given picture to the given thickness. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = penWidth(10) -> p
        draw(pic)
      </pre>
    </div>,
    "hue" -> 
    <div>
      <strong>hue</strong>(factor) -> picture <br/>
      Changes the hue of the given picture's fill color by the given factor. <br/>
      The factor needs to be between -1 and 1. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = hue(0.5) * fillColor(blue) -> p
        // val pic = hue(-0.5) * fillColor(blue) -> p
        draw(pic)
      </pre>
    </div>,
    "sat" -> 
    <div>
      <strong>sat</strong>(factor) -> picture <br/>
      Changes the saturation of the given picture's fill color by the given factor. <br/>
      The factor needs to be between -1 and 1. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = sat(-0.5) * fillColor(blue) -> p
        draw(pic)
      </pre>
    </div>,
    "brit" -> 
    <div>
      <strong>brit</strong>(factor) -> picture <br/>
      Changes the brightness of the given picture's fill color by the given factor.<br/>
      The factor needs to be between -1 and 1. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = brit(-0.5) * fillColor(blue) -> p
        draw(pic)
      </pre>
    </div>,
    "opac" -> 
    <div>
      <strong>opac</strong>(factor) -> picture <br/>
      Changes the opacity of the given picture by the given factor.<br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = opac(-0.5) * fillColor(blue) -> p
        draw(pic)
      </pre>
    </div>,
    "hueMod" -> 
    <div>
      <strong>hueMod</strong>(color, factor)<br/>
      A function that computes and returns a new color made by changing the hue of the 
      given color by the given factor.<br/>
      The factor needs to be between -1 and 1. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
def square(n: Int) {{
    repeat(4) {{
        forward(n)
        right()
    }}
}}

clear()
setAnimationDelay(10)
var fillC = cyan
repeat(5) {{
    setFillColor(fillC)
    square(50)
    penUp()
    right()
    forward(60)
    left()
    penDown()
    fillC = hueMod(fillC, 0.25)
    // also try -0.25 as change factor
}}
      </pre>
    </div>,
    "satMod" -> 
    <div>
      <strong>satMod</strong>(color, factor)<br/>
      A function that computes and returns a new color made by changing the saturation of the 
      given color by the given factor.<br/>
      The factor needs to be between -1 and 1. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
def square(n: Int) {{
    repeat(4) {{
        forward(n)
        right()
    }}
}}

clear()
setAnimationDelay(10)
var fillC = cyan
repeat(5) {{
    setFillColor(fillC)
    square(50)
    penUp()
    right()
    forward(60)
    left()
    penDown()
    fillC = satMod(fillC, -0.25)
}}
      </pre>
    </div>,
    "britMod" -> 
    <div>
      <strong>britMod</strong>(color, factor)<br/>
      A function that computes and returns a new color made by changing the brightness of the 
      given color by the given factor.<br/>
      The factor needs to be between -1 and 1. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
def square(n: Int) {{
    repeat(4) {{
        forward(n)
        right()
    }}
}}

clear()
setAnimationDelay(10)
var fillC = cyan
repeat(5) {{
    setFillColor(fillC)
    square(50)
    penUp()
    right()
    forward(60)
    left()
    penDown()
    fillC = britMod(fillC, -0.25)
}}
      </pre>
    </div>,
    "axes" -> 
    <div>
      <strong>axes</strong> -> picture <br/>
      Turns on local axes for the picture (to help during picture construction). <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        val pic = axes * fillColor(blue) -> p
        draw(pic)
      </pre>
    </div>,
    "flipY" -> 
    <div>
      <strong>flipY</strong> -> picture <br/>
      Flips the given picture around the local Y axis. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        axesOn()
        val pic = trans(100, 0) * flipY * fillColor(blue) -> p
        draw(pic)
      </pre>
    </div>,
    "flipX" -> 
    <div>
      <strong>flipX</strong> -> picture <br/>
      Flips the given picture around the local X axis. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        axesOn()
        val pic = trans(100, 0) * flipX * fillColor(blue) -> p
        draw(pic)
      </pre>
    </div>,
    "flip" -> 
    <div>
      <strong>flip</strong> -> picture <br/>
      The same thing as flipY. <br/>
      Flips the given picture around the local Y axis. <br/>
      <br/>
      <em>Example:</em> <br/>
      <br/>
      <pre>
        def p = Picture {{
          repeat (4) {{
            forward(50)
            right()
          }}
        }}

        clear()
        invisible()
        axesOn()
        val pic = trans(100, 0) * flip * fillColor(blue) -> p
        draw(pic)
      </pre>
    </div>,
    "animate" -> 
    <div>
       <strong>animate</strong> {{ code }} <br/><br/>
       Calls the given code block repeatedly within an animation loop. The refresh rate,
       specified with the <tt>setRefreshRate()</tt> command, specifies the number of times per 
       second that the code is run. This value can be anywhere between 20 and 100 (times 
       per second).
    </div>,
    "schedule" -> 
    <div>
       <strong>schedule</strong>(seconds) {{ code }} <br/><br/>
       Schedules the given code block to be called (in the animation/GUI thread) after the specified 
       number of seconds.
    </div>,
    "stClear" -> "stClear() - Clears the Story Teller Window.",
    "stPlayStory" -> "stPlayStory(story) - Plays the given story.",
    "stFormula" -> "stFormula(latex) - Converts the supplied latex string into html that can be displayed in the Story Teller Window.",
    "stPlayMp3" -> "stPlayMp3(fileName) - Plays the specified MP3 file.",
    "stPlayMp3Loop" -> "stPlayMp3Loop(fileName) - Plays the specified MP3 file in the background.",
    "stAddButton" -> "stAddButton(label) {code} - Adds a button with the given label to the Story Teller Window, and runs the supplied code when the button is clicked.",
    "stAddField" -> "stAddField(label, default) - Adds an input field with the supplied label and default value to the Story Teller Window.",
    "stFieldValue" -> "stFieldValue(label, default) - Gets the value of the specified field.",
    "stShowStatusMsg" -> "stShowStatusMsg(msg) - Shows the specified message in the Story Teller status bar.",
    "stSetScript" -> "stSetScript(code) - Copies the supplied code to the script editor.",
    "stRunCode" -> "stRunCode(code) - Runs the supplied code (without copying it to the script editor).",
    "stClickRunButton" -> "stClickRunButton() - Simulates a click of the run button.",
    "stShowStatusError" -> "stShowStatusError(msg) - Shows the specified error message in the Story Teller status bar.",
    "stNext" -> "stNext() - Moves the story to the next page/view."
  )
  
  val StagingContent = Map[String, String](
    "clear" -> "clear() - Clears the canvas.",
    "clearWithUL" -> "clearWithUL(unit) - Clears the canvas, and sets the given unit length (Pixel, Cm, or Inch)."
  )

  val MwContent = Map[String, String]()
  
  val D3Content = Map[String, String](
    "clear" -> "clear() - Clears the 3D Canvas and moves the 3D Turtle back to the default initial position.",
    "render" -> "render() - Forces the camera to make a photo and display it on the 3D Canvas. Useful when intermediate rendering is turned off with the renderOnRequest() command.",
    "renderAlways" -> "renderAlways() - Tells the camera to refresh the picture on the 3D Canvas every time something visible changes. The opposite of renderOnRequest()",
    "renderOnRequest" -> "renderOnRequest() - Tells the camera to refresh the picture on the 3D Canvas only when you request it by calling the render() command. Useful when you only want to see the end result of the script. The opposite of renderAlways()",
    "sphere" -> "sphere(radius) - Creates a sphere of the given radius, centered on the current turtle's position.",
    "cylinder" -> "cylinder(radius, height) - Creates a cylinder of the given radius and height, with one end where the 3D Turtle is, and the other end pointing in the forward direction of the 3D Turtle.",
    "plane" -> "plane() - Creates a plane perpendicular to the 3D Turtle's forward direction, which includes the current 3D Turtle's position.",
    "cube" -> "cube(dimension) - Creates a cube with all three dimensions equal to the given distance, centered around the 3D Turtle, with faces aligned with 3D Turtle's front/up/side directions.",
    "pointLight" -> "pointLight(r, g, b, x, y, z) - Creates a new light source which shines with a color determined by the given red, green and blue components. It is located at the given x, y and z coordinates.",
    "forward" -> "forward(distance) - Moves the 3D Turtle forward by a given distance. If trail is turned on, the 3D Turtle will leave a line (thin cyllinder) behind.",
    "back" -> "back(distance) - Moves the 3D Turtle backwards by a given distance. Equivalent to forward(-distance).",
    "turn" -> "turn(angle) - Rotates the 3D Turtle around his local Y axis (the turtle's up direction). Positive angle values mean counterclockwise rotation - turning left. Negative angle values turn the 3D Turtle clockwise - right.",
    "left" -> "left(angle) - Rotates the 3D Turtle around his local Y axis. Positive angle values make the 3D Turtle turn left. Equivalent to turn(angle)",
    "right" -> "right(angle) - Rotates the 3D Turtle around his local Y axis. Positive angle values make the 3D Turtle turn right. Equivalent to turn(-angle)",
    "pitch" -> "pitch(angle) - Rotates the 3D Turtle around his local X axis (the turtle's left direction). Positive angle values make the 3D Turtle look down. Negative angle values make the 3D Turtle look up.",
    "roll" -> "roll(angle) - Rotates the 3D Turtle around his local Z axis (the turtle's forward direction). Positive angle values make the 3D Turtle lift his left shoulder and lower the right one. Negative angle values make the 3D Turtle lift his right shoulder and lower the left one.",
    "moveTo" -> "moveTo(x, y, z) - Moves the 3D Turtle to the given coordinates without changing his orientation and without drawing lines.",
    "lookAt" -> "lookAt(x, y, z) - Makes the 3D Turtle rotate so that it is pointing towards the given point.",
    "strafeUp" -> "strafeUp(distance) - Moves the 3D Turtle upwards by the given distance. If trail is turned on, the 3D Turtle will leave a line behind.",
    "strafeDown" -> "strafeDown(distance) - Moves the 3D Turtle downwards by the given distance. If trail is turned on, the 3D Turtle will leave a line behind.",
    "strafeLeft" -> "strafeLeft(distance) - Moves the 3D Turtle left by the given distance. If trail is turned on, the 3D Turtle will leave a line behind.",
    "strafeRight" -> "strafeRight(distance) - Moves the 3D Turtle right by the given distance. If trail is turned on, the 3D Turtle will leave a line behind.",
    "invisible" -> "invisible() - Makes the 3D Turtle invisible. Use the visible() command to make him visible again.",
    "visible" -> "visible() - Makes the 3D Turtle visible after he was made invisible with the invisible() command.",
    "trailOn" -> "trailOn() - Makes the 3D Turtle leave lines (thin cylinders) behind as he moves around. Us the trailOff() command to stop the 3D Turtle from drawing any more lines.",
    "trailOff" -> "trailOff() - Causes the 3D Turtle to no longer leave lines as he moves around. Use the trailOn() command to turn them on again.",
    "lineWidth" -> "lineWidth(width) - Sets the width of the lines left by the 3D Turtle when he moves around with trail turned on.",
    "color" -> "color(r, g, b) - Sets the color of lines left by the 3D Turtle as well as shapes created by him. Specify the color by giving the red, green and blue components either as an integer from 0 to 255, or as a fraction from 0 to 1. Some colors can be specified by name, eg. color(red).",
    "cameraForward" -> "cameraForward(distance) - Moves the 3D camera forward by the given distance.",
    "cameraBack" -> "cameraBack(distance) - Moves the 3D camera back by the given distance.",
    "cameraTurn" -> "cameraTurn(angle) - Rotates the 3D camera around it's local Y axis. Positive angle values turn the camera left. Negative angle values turn the camera right. Notice that rotating the camera causes the picture to rotate in the opposite direction.",
    "cameraLeft" -> "cameraLeft(angle) - Rotates the 3D camera around it's local Y axis. Positive angle values turn the camera left. Equivalent to cameraTurn(angle). Notice that rotating the camera causes the picture to rotate in the opposite direction.",
    "cameraRight" -> "cameraRight(angle) - Rotates the 3D camera around it's local Y axis. Positive angle values turn the camera right. Equivalent to cameraTurn(-angle). Notice that rotating the camera causes the picture to rotate in the opposite direction.",
    "cameraPitch" -> "cameraPitch(angle) - Rotates the 3D camera around it's local X axis. Positive angle values turn the camera down. Negative angle values turn the camera up. Notice that rotating the camera causes the picture to rotate in the opposite direction.",
    "cameraRoll" -> "cameraRoll(angle) - Rotates the 3D camera around it's local Z axis. Positive angle values roll the camera clockwise. Negative angle values turn the camera counterclockwise. Notice that rotating the camera causes the picture to rotate in the opposite direction.",
    "cameraStrafeUp" -> "cameraStrafeUp(distance) - Moves the 3D camera upwards by the given distance.",
    "cameraStrafeDown" -> "cameraStrafeDown(distance) - Moves the 3D camera downwards by the given distance.",
    "cameraStrafeLeft" -> "cameraStrafeLeft(distance) - Moves the 3D camera left by the given distance.",
    "cameraStrafeRight" -> "cameraStrafeRight(distance) - Moves the 3D camera right by the given distance.",
    "cameraMoveTo" -> "cameraMoveTo(x, y, z) - Moves the 3D camera to the given coordinates without changing it's orientation.",
    "cameraLookAt" -> "cameraLookAt(x, y, z) - Rotates the 3D camera so that the given point is directly in the center of the picture.",
    "cameraAngle" -> "cameraAngle(angle) - Changes the focus of the 3D camera. An angle of 30 degrees means that the Camera will see everything within 15 degrees to the left and 15 degrees to the right. Low values make the pictures look as if taken through a telescope. Allowed values are between 0 and 180 degrees, althoug you normally shouldn't use values too close to these limits. The default value is 90 degrees.",
    "enableMouseControl" -> "enableMouseControl() - Makes it possible to rotate and move the 3D camera by dragging the 3D Canvas. Movement and rotation can be affected by modifier keys - Shift, Alt and Ctrl.",
    "disableMouseControl" -> "disableMouseControl() - Makes it no longer possible to control the camera by dragging the 3D Canvas. Useful when you want your Kojo script to be a non-interactive animation.",
    "axesOn" -> "axesOn() - Makes the axes visible after they were made invisible with the axesOff() command.",
    "axesOff" -> "axesOff() - Makes the axes invisible. You can make them visible again with the axesOn() command.",
    "defaultLightsOn" -> "defaultLightsOn() - Turns on the default lights after they were turned off with the defaultLightsOff() command.",
    "defaultLightsOff" -> "defaultLightsOff() - Turns off the default lights. You can turn them back on with the defaultLightsOn() command.",
    "imageInterpolationOn" -> "imageInterpolationOn() - Makes the picture blurry to remove the effect of big pixels visible in lower qualities.",
    "imageInterpolationOff" -> "imageInterpolationOff() - Makes the picture no longer blurry, but in lower qualities you may see big pixels making up the picture.",
    "enableOrthographicMode" -> "enableOrthographicMode() - Switches the 3D camera into orthographic mode. In this mode, the 3D camera is infinitely distant as if you were watching everything through a very powerful telescope from a very distant place. In this mode you can no longer see the effects of perspective.",
    "disableOrthographicMode" -> "disableOrthographicMode() - Switches the 3D camera into perspective mode. In this mode you can see the effects of perspective (objects getting smaller as they move away from the 3D camera)."
  )

  @volatile var modeSpecificContent: Map[String, String] = TwContent
  
  def activateTw() {
    modeSpecificContent = TwContent
    clearLangContent()
  }

  def activateMw() {
    modeSpecificContent = MwContent
    clearLangContent()
  }

  def activateStaging() {
    modeSpecificContent = StagingContent
    clearLangContent()
  }

  def activateD3() {
    modeSpecificContent = D3Content
    clearLangContent()
  }

  val langContent: collection.mutable.Map[String, Map[String, String]] = collection.mutable.Map()
  def addContent(lang: String, content: Map[String, String]) {
//    import util.Typeclasses._
//    langContent +=  (lang -> (langContent.getOrElse(lang, Map()) |+| content))
    langContent +=  (lang -> (langContent.getOrElse(lang, Map()) ++ content))
  }
  
  def clearLangContent() {
    langContent.clear()
  }
  
  def langHelp(name: String, lang: String): Option[String] = {
    langContent.get(lang) match {
      case Some(content) => content.get(name)
      case None => None
    }
  }
  
  def apply(topic: String) = {
    CommonContent.getOrElse(
      topic, 
      modeSpecificContent.getOrElse(
        topic, 
        langHelp(topic, System.getProperty("user.language")).getOrElse("Coming Soon...")
      )
    )
  }
}
