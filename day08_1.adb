with Ada.Text_IO;

procedure Day08_1 is
   subtype Tree_Row is String (1 .. 99);
   type Plantation is array (Tree_Row'Range) of Tree_Row;

   function Visible (Field : in Plantation; Row : in Positive; Column : in Positive) return Boolean with
      Pre => Row in Tree_Row'Range and Column in Tree_Row'Range;
   -- Returns True if the tree at (Row, Column) in Field is visible; False otherwise

   function Visible (Field : in Plantation; Row : in Positive; Column : in Positive) return Boolean is
      function Visible_Left   return Boolean;
      function Visible_Top    return Boolean;
      function Visible_Right  return Boolean;
      function Visible_Bottom return Boolean;

      function Visible_Left return Boolean is
         -- Empty
      begin -- Visible_Left
         Check : for I in 1 .. Column - 1 loop
            if Field (Row) (I) >= Field (Row) (Column) then
               return False;
            end if;
         end loop Check;

         return True;
      end Visible_Left;

      function Visible_Top return Boolean is
         -- Empty
      begin -- Visible_Top
         Check : for I in 1 .. Row - 1 loop
            if Field (I) (Column) >= Field (Row) (Column) then
               return False;
            end if;
         end loop Check;

         return True;
      end Visible_Top;

      function Visible_Right return Boolean is
         -- Empty
      begin -- Visible_Right
         Check : for I in Column + 1 .. Tree_Row'Last loop
            if Field (Row) (I) >= Field (Row) (Column) then
               return False;
            end if;
         end loop Check;

         return True;
      end Visible_Right;

      function Visible_Bottom return Boolean is
         -- Empty
      begin -- Visible_Bottom
         Check : for I in Row + 1 ..Tree_Row'Last loop
            if Field (I) (Column) >= Field (Row) (Column) then
               return False;
            end if;
         end loop Check;

         return True;
      end Visible_Bottom;
   begin -- Visible
      return Visible_Left or else Visible_Top or else Visible_Right or else Visible_Bottom;
   end Visible;

   Input : Ada.Text_IO.File_Type;
   Field : Plantation;
   Count : Positive := 4 * Tree_Row'Length - 4; -- All edge trees are visible
begin -- Day08_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_08");

   Read_Field : for I in Field'Range loop
      Ada.Text_IO.Get (File => Input, Item => Field (I) );
      Ada.Text_IO.Skip_Line (File => Input);
   end loop Read_Field;

   Ada.Text_IO.Close (File => Input);

   Check_Rows : for Row in Field'First + 1 .. Field'Last - 1 loop
      Check_Columns : for Column in Field'First + 1 .. Field'Last - 1 loop
         if Visible (Field, Row, Column) then
            Count := Count + 1;
         end if;
      end loop Check_Columns;
   end loop Check_Rows;

   Ada.Text_IO.Put_Line (Item => Count'Image);
end Day08_1;
