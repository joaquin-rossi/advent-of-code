with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Containers;
use Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Strings.Unbounded;
use Ada.Text_IO;

procedure Day_05 is
    package Maps is new
       Ada.Containers.Ordered_Maps
          (Key_Type     => Big_Integer,
           Element_Type => Big_Integer);

    type Interval_Set is record
        M : Maps.Map;
    end record;

    procedure Insert (S : in out Interval_Set; L, R : Big_Integer) is
        use Maps;
        New_L : Big_Integer := L;
        New_R : Big_Integer := R;
        It    : Cursor;
    begin
        if L > R then
            raise Constraint_Error with "Invalid interval: L > R";
        end if;

        It := Floor (S.M, L);
        if It = No_Element then
            It := Ceiling (S.M, L);
        end if;

        while It /= No_Element loop
            declare
                Cur_L : constant Big_Integer := Key (It);
                Cur_R : constant Big_Integer := Element (It);
            begin
                if Cur_L > New_R + 1 then
                    exit;
                end if;

                if Cur_R < New_L - 1 then
                    It := Next (It);
                else
                    if Cur_L < New_L then
                        New_L := Cur_L;
                    end if;
                    if Cur_R > New_R then
                        New_R := Cur_R;
                    end if;

                    declare
                        Next_It : constant Cursor := Next (It);
                    begin
                        Delete (S.M, It);
                        It := Next_It;
                    end;
                end if;
            end;
        end loop;

        Insert (S.M, New_L, New_R);
    end;

    function Contains (S : Interval_Set; X : Big_Integer) return Boolean is
        use Maps;
        It : Cursor;
    begin
        It := Floor (S.M, X);
        if It = No_Element then
            return False;
        end if;
        return Element (It) >= X;
    end;

    function Size (S : Interval_Set) return Big_Integer is
        use Maps;
        It     : Cursor := First (S.M);
        Result : Big_Integer := 0;
    begin
        while It /= No_Element loop
            declare
                L : constant Big_Integer := Key (It);
                R : constant Big_Integer := Element (It);
            begin
                Result := Result + R - L + 1;
            end;
            It := Next (It);
        end loop;

        return Result;
    end;

    Input_Line   : Unbounded_String;
    Input_Part   : Integer := 1;
    Input_Int_Lo : Big_Integer;
    Input_Int_Hi : Big_Integer;
    Input_Num    : Big_Integer;

    Intervals : Interval_Set;

    Silver : Big_Integer := 0;
    Gold   : Big_Integer := 0;
begin
    while not End_Of_File loop
        Input_Line := To_Unbounded_String (Get_Line);

        if Length (Input_Line) = 0 then
            Input_Part := Input_Part + 1;
            Gold := Size (Intervals);
        else
            case Input_Part is
                when 1      =>
                    Input_Int_Lo :=
                       From_String
                          (Slice (Input_Line, 1, Index (Input_Line, "-") - 1));

                    Input_Int_Hi :=
                       From_String
                          (Slice
                              (Input_Line,
                               Index (Input_Line, "-") + 1,
                               Length (Input_Line)));

                    Insert (Intervals, Input_Int_Lo, Input_Int_Hi);

                when 2      =>
                    Input_Num :=
                       From_String
                          (Slice (Input_Line, 1, Length (Input_Line)));

                    if Contains (Intervals, Input_Num) then
                        Silver := Silver + 1;
                    end if;

                when others =>
                    raise Constraint_Error;
            end case;
        end if;
    end loop;

    Put_Line ("Silver:" & Silver'Image);
    Put_Line ("Gold:" & Gold'Image);
end;
