with Ada.Text_IO;

procedure Day08_2 is
   subtype Tree_Row is String (1 .. 99);
   type Plantation is array (Tree_Row'Range) of Tree_Row;

   function Score (Field : in Plantation; Row : in Positive; Column : in Positive) return Natural with
      Pre => Row in Tree_Row'Range and Column in Tree_Row'Range;
   -- Returns the score for the tree at (Row, Column) in Field

   function Score (Field : in Plantation; Row : in Positive; Column : in Positive) return Natural is
      function Score_Left   return Natural;
      function Score_Top    return Natural;
      function Score_Right  return Natural;
      function Score_Bottom return Natural;

      function Score_Left return Natural is
         -- Empty
      begin -- Score_Left
         Check : for I in reverse 1 .. Column - 1 loop
            if Field (Row) (I) >= Field (Row) (Column) then
               return Column - I;
            end if;
         end loop Check;

         return Column - 1;
      end Score_Left;

      function Score_Top return Natural is
         -- Empty
      begin -- Score_Top
         Check : for I in reverse 1 .. Row - 1 loop
            if Field (I) (Column) >= Field (Row) (Column) then
               return Row - I;
            end if;
         end loop Check;

         return Row - 1;
      end Score_Top;

      function Score_Right return Natural is
         -- Empty
      begin -- Score_Right
         Check : for I in Column + 1 .. Tree_Row'Last loop
            if Field (Row) (I) >= Field (Row) (Column) then
               return  I - Column;
            end if;
         end loop Check;

         return Tree_Row'Last - Column;
      end Score_Right;

      function Score_Bottom return Natural is
         -- Empty
      begin -- Score_Bottom
         Check : for I in Row + 1 ..Tree_Row'Last loop
            if Field (I) (Column) >= Field (Row) (Column) then
               return I - Row;
            end if;
         end loop Check;

         return Tree_Row'Last - Row;
      end Score_Bottom;
   begin -- Score
      return Score_Left * Score_Top * Score_Right * Score_Bottom;
   end Score;

   Input : Ada.Text_IO.File_Type;
   Field : Plantation;
   Max   : Natural := 0;
begin -- Day08_2
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_08");

   Read_Field : for I in Field'Range loop
      Ada.Text_IO.Get (File => Input, Item => Field (I) );
      Ada.Text_IO.Skip_Line (File => Input);
   end loop Read_Field;

   Ada.Text_IO.Close (File => Input);

   Check_Rows : for Row in Field'First + 1 .. Field'Last - 1 loop
      Check_Columns : for Column in Field'First + 1 .. Field'Last - 1 loop
         Max := Integer'Max (Score (Field, Row, Column), Max);
      end loop Check_Columns;
   end loop Check_Rows;

   Ada.Text_IO.Put_Line (Item => Max'Image);
end Day08_2;
