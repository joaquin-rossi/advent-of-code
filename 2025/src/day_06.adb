with Ada.Containers.Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Containers;
use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada.Text_IO;

procedure Day_06 is
    package String_Vec is new
       Vectors (Index_Type => Positive, Element_Type => Unbounded_String);
    use String_Vec;

    Input_Lines : String_Vec.Vector;

    Silver : Big_Integer := 0;

    procedure Silver_Count is
        type Op is (Add, Mul);

        package Op_Vec is new
           Vectors (Index_Type => Positive, Element_Type => Op);
        use Op_Vec;

        package BI_Vec is new
           Vectors (Index_Type => Positive, Element_Type => Big_Integer);
        use BI_Vec;

        Ops  : Op_Vec.Vector;
        Vals : BI_Vec.Vector;

        Num_Line_C : String_Vec.Cursor := First (Input_Lines);
        Num_Line   : Unbounded_String;
        Num_I      : Positive;
        Cur_I      : Positive;
        Cur_C      : Character;
        Num_End    : Natural;
        Num_Val    : Big_Integer;
    begin
        declare
            Op_Line : Unbounded_String := Element (Last (Input_Lines));
        begin
            for Cur_I in 1 .. Length (Op_Line) loop
                case Element (Op_Line, Cur_I) is
                    when '+'    =>
                        Append (Ops, Add);
                        Append (Vals, 0);

                    when '*'    =>
                        Append (Ops, Mul);
                        Append (Vals, 1);

                    when others =>
                        null;
                end case;
            end loop;
        end;

        while Num_Line_C /= Last (Input_Lines) loop
            Num_Line := Element (Num_Line_C);

            Num_I := 1;
            Cur_I := 1;

            while Cur_I <= Length (Num_Line) loop
                Cur_C := Element (Num_Line, Cur_I);

                if Cur_C /= ' ' then
                    Num_End := Index (Num_Line, " ", From => Cur_I);

                    if Num_End > 0 then
                        Num_Val :=
                           From_String (Slice (Num_Line, Cur_I, Num_End));
                        Cur_I := Num_End;
                    else
                        Num_Val :=
                           From_String
                              (Slice (Num_Line, Cur_I, Length (Num_Line)));
                        Cur_I := Length (Num_Line);
                    end if;

                    case Ops (Num_I) is
                        when Add =>
                            Vals (Num_I) := Vals (Num_I) + Num_Val;

                        when Mul =>
                            Vals (Num_I) := Vals (Num_I) * Num_Val;
                    end case;
                    Num_I := Num_I + 1;
                end if;

                Cur_I := Cur_I + 1;
            end loop;

            Next (Num_Line_C);
        end loop;

        for Cur_C in Iterate (Vals) loop
            Silver := Silver + Element (Cur_C);
        end loop;
    end;

    Gold : Big_Integer := 0;

    procedure Gold_Count is
        Op_Line : Unbounded_String := Element (Last (Input_Lines));
        Cur_C   : Character;

        Op_End : Positive;
        Op_C   : Character;
        Op_NC  : Integer;
        Op_N   : Big_Integer;
        Op_T   : Big_Integer;
    begin
        for Cur_I in 1 .. Length (Op_Line) loop
            Cur_C := Element (Op_Line, Cur_I);
            if Cur_C = '+' or Cur_C = '*' then
                Op_End := Cur_I + 1;
                while Op_End <= Length (Op_Line)
                   and then Element (Op_Line, Op_End) = ' '
                loop
                    Op_End := Op_End + 1;
                end loop;

                if Op_End <= Length (Op_Line) then
                    Op_End := Op_End - 2;
                else
                    Op_End := Op_End - 1;
                end if;

                if Cur_C = '+' then
                    Op_T := 0;
                else
                    Op_T := 1;
                end if;

                for Op_I in Cur_I .. Op_End loop
                    Op_N := 0;

                    for Line_I in 1 .. Length (Input_Lines) - 1 loop
                        Op_C :=
                           Element
                              (Input_Lines (Positive (Line_I)),
                               Positive (Op_I));

                        if Op_C /= ' ' then
                            Op_NC :=
                               Character'Pos (Op_C) - Character'Pos ('0');
                            Op_N := 10 * Op_N + To_Big_Integer (Op_NC);
                        end if;
                    end loop;

                    if Cur_C = '+' then
                        Op_T := Op_T + Op_N;
                    else
                        Op_T := Op_T * Op_N;
                    end if;
                end loop;

                Gold := Gold + Op_T;
            end if;
        end loop;
    end;

begin
    while not End_Of_File loop
        declare
            Input_Line : Unbounded_String := To_Unbounded_String (Get_Line);
        begin
            Append (Input_Lines, Input_Line);
        end;
    end loop;

    Silver_Count;
    Put_Line ("Silver: " & Silver'Image);

    Gold_Count;
    Put_Line ("Gold: " & Gold'Image);
end;
