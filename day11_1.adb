with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Day11_1 is
   subtype Monkey_ID is Integer range 0 .. 7;

   type Item_Value is mod 2 ** 64;

   package Item_Lists is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Item_Value);

   type Operand_Info (Self : Boolean := True) is record
      case Self is
      when False =>
         Value : Item_Value;
      when True =>
         null;
      end case;
   end record;

   type Destination_List is array (Boolean) of Monkey_ID;

   type Monkey_Info is record
      Item        : Item_Lists.Vector;
      Operation   : Character;
      Operand     : Operand_Info;
      Modulus     : Item_Value;
      Destination : Destination_List;
      Inspections : Natural := 0;
   end record;

   type Monkey_List is array (Monkey_ID) of Monkey_Info;

   procedure Apply_Round (ID : in Monkey_ID; Monkey : in out Monkey_List);
   -- Performs a round for monkey ID, emptying Item and updating Inspections

   procedure Apply_Round (ID : in Monkey_ID; Monkey : in out Monkey_List) is
      Item    : Item_Value;
      Operand : Item_Value;
   begin -- Apply_Round
      All_Items : loop
         exit All_Items when Monkey (ID).Item.Is_Empty;

         Item := Monkey (ID).Item.First_Element;
         Monkey (ID).Item.Delete_First;
         Monkey (ID).Inspections := Monkey (ID).Inspections + 1;
         Operand := (if Monkey (ID).Operand.Self then Item else Monkey (ID).Operand.Value);

         if Monkey (ID).Operation = '+' then
            Item := Item + Operand;
         else
            Item := Item * Operand;
         end if;

         Item := Item / 3;
         Monkey (Monkey (ID).Destination (Item rem Monkey (ID).Modulus = 0) ).Item.Append (New_Item => Item);
      end loop All_Items;
   end Apply_Round;

   type Max_Info is record
      ID    : Monkey_ID;
      Value : Natural := 0;
   end record;

   Input  : Ada.Text_IO.File_Type;
   Monkey : Monkey_List;
   Max_1  : Max_Info;
   Max_2  : Max_Info;
begin -- Day11_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_11");

   Read_Monkeys : for ID in Monkey'Range loop
      exit Read_Monkeys when Ada.Text_IO.End_Of_File (Input);

      All_Lines : for L in 1 .. 7 loop
         exit Read_Monkeys when ID = Monkey'Last and L = 7;

         One_Line : declare
            Line : constant String := Ada.Text_IO.Get_Line (Input);
         begin -- One_Line
            case L is
            when 1 | 7 => -- Monkey # & final null line; ignore
               null;
            when 2 => -- Item list
               Parse_List : declare
                  Start : Positive := Ada.Strings.Fixed.Index (Line, ":") + 1;
                  Stop  : Natural;
               begin -- Parse_List
                  Parse_All : loop
                     exit Parse_All when Start not in Line'Range;

                     Stop := Ada.Strings.Fixed.Index (Line (Start .. Line'Last), ",");
                     Stop := (if Stop = 0 then Line'Last else Stop - 1);
                     Monkey (ID).Item.Append (New_Item => Item_Value'Value (Line (Start .. Stop) ) );
                     Start := Stop + 2;
                  end loop Parse_All;
               end Parse_List;
            when 3 => -- Operation & operand
               Parse_Op : declare
                  Pos : constant Positive := Ada.Strings.Fixed.Index (Line, "old") + 4; -- Operator position
               begin -- Parse_Op
                  Monkey (ID).Operation := Line (Pos);

                  if Line (Pos + 2) = 'o' then -- Operand is "old"
                     Monkey (ID).Operand := (Self => True);
                  else -- Operand is a number
                     Monkey (ID).Operand := (Self => False, Value => Item_Value'Value (Line (Pos + 2 .. Line'Last) ) );
                  end if;
               end Parse_Op;
            when 4 => -- Modulus
               Parse_Mod : declare
                  Pos : constant Positive := Ada.Strings.Fixed.Index (Line, " ", Going => Ada.Strings.Backward);
               begin -- Parse_Mod
                  Monkey (ID).Modulus := Item_Value'Value (Line (Pos .. Line'Last) );
               end Parse_Mod;
            when 5 .. 6 => -- Destinations
               Parse_Dest : declare
                  Pos : constant Positive := Ada.Strings.Fixed.Index (Line, " ", Going => Ada.Strings.Backward);
               begin -- Parse_Dest
                  Monkey (ID).Destination (L = 5) := Integer'Value (Line (Pos .. Line'Last) );
               end Parse_Dest;
            end case;
         end One_Line;
      end loop All_Lines;
   end loop Read_Monkeys;

   Ada.Text_IO.Close (File => Input);

   All_Rounds : for R in 1 .. 20 loop
      All_Monkeys : for ID in Monkey'Range loop
         Apply_Round (ID => ID, Monkey => Monkey);
      end loop All_Monkeys;
   end loop All_Rounds;

   Find_1 : for ID in Monkey'Range loop
      if Monkey (ID).Inspections > Max_1.Value then
         Max_1 := (ID => ID, Value => Monkey (ID).Inspections);
      end if;
   end loop Find_1;

   Find_2 : for ID in Monkey'Range loop
      if ID /= Max_1.ID and Monkey (ID).Inspections > Max_2.Value then
         Max_2 := (ID => ID, Value => Monkey (ID).Inspections);
      end if;
   end loop Find_2;

   Ada.Text_IO.Put_Line (Item => Max_1.ID'Image & Max_1.Value'Image);
   Ada.Text_IO.Put_Line (Item => Max_2.ID'Image & Max_2.Value'Image);
   Ada.Text_IO.Put_Line (Item => Integer'Image (Max_1.Value * Max_2.Value) );
end Day11_1;
