with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;
with Ada.Tags; use Ada.Tags;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed; 

procedure Simulator is

    subtype Id_T is String (1 .. 5);

    type Component_T is tagged null record;
    type Component_Access is access all Component_T'Class;
    type Components_T is array (Natural range <>) of Component_Access;

    type Entity_T (Count : Positive) is tagged record
        Id         : Id_T;
        Components : Components_T (0 .. Count);
    end record;
    type Entity_Access is access all Entity_T'Class;
    type Entities_T    is array (Natural range <>) of Entity_Access;

    function Get_Components (E: Entity_T'Class; Tag: Ada.Tags.Tag) return Components_T is
        Count : Natural := 0;
    begin
        for C of E.Components loop
            Count := (if C'Tag = Tag then Count + 1 else count);
        end loop;
        declare
            Result : Components_T (0 .. Count-1);
        begin
            Count := 0;
            for C of E.Components loop
                if C'Tag = Tag then
                    Result (Count) := C;
                    Count := Count + 1;
                end if;
            end loop;
            return Result;
        end;
    end;

    type System_T is interface;
    procedure Execute (Self : System_T;
                       Dt   : Duration; 
                       E    : in out Entity_T;  
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is abstract;
    type System_Access is access all System_T'Class;

    type Transform_T is new Component_T with record
        X, Y : Float;
    end record;

    type Origin_T is (Top_Left, Bottom_Left, Center, Bottom_Right, Top_Right);

    type Char_Image_T is array (Natural range <>, Natural range <>) of Character;
    type Char_Image_Access is access all Char_Image_T;
    type Image_T (H : Natural; W : Natural) is new Component_T with record
        Image : Char_Image_T (1 .. H, 1 .. W);
        O     : Origin_T;
    end record;
    type Image_Access is access all Image_T;

    type Anim_Count_T is mod 2;
    type Frames_T is array (Anim_Count_T) of Image_Access;
    type Animation_T is new Component_T with record
        Frames  : Frames_T;
        Current : Anim_Count_T;
    end record;

    type Body_T is new Component_T with record
        X0   : Float := 18.0;
        Y0   : Float := 3.0;
        VX0  : Float := 1.0;
        VY0  : Float := 2.0;
        FX   : Float := 1.0;
        FY   : Float := 0.0;
        Mass : Float := 2_000_000.0;
    end record;

    type Control_T is record
        Thrust_X, Thrust_Y : Float;
    end record;

    type Sequence_Idx_T is range 0 .. 53;
    type Controls_T is array (Sequence_Idx_T'Range) of Control_T;
    type Shuttle_Controls_T is new Component_T with record
        Sequence : Controls_T;
        Idx      : Sequence_Idx_T;
    end record;

    type Color_Name_T is (White, Gray, Light_Gray, Green, Blue, Yellow, Magenta, Pink, Orange);

    D : constant := 1.0 / 256.0;
    type Channel is delta D range 0.0 .. 1.0 with size => 8;
    function Max return Channel is
        (1.0 - D);

    type Color_T is record
        R, G, B : Channel;
    end record
        with size => 32;

    type Colors_T is array (Color_Name_T) of Color_T;
    Colors : Colors_T := (White      => (Max, Max, Max),
                          Gray       => (0.5, 0.5, 0.5),
                          Light_Gray => (0.7, 0.7, 0.7),
                          Green      => (0.0, Max, 0.0),
                          Blue       => (0.0, 0.0, Max),
                          Yellow     => (Max, Max, 0.0),
                          Magenta    => (Max, 0.0, Max),
                          Pink       => (0.78, 0.43, 0.53),
                          Orange     => (Max, 0.5, 0.0));

    type Colors_Map_T is array (Character'Range) of Color_Name_T;
    type Color_Map_T is new Component_T with record
        CM : Colors_Map_T;
    end record;

    type Star_Indices_T is array (Natural range <>) of Natural;
    type Transforms_T is array (Natural range <>) of Transform_T;
    type Random_Stars_T (Count : Natural) is new Component_T with record
        I : Star_Indices_T (1 .. Count);
        T : aliased Transforms_T (1 .. Count);
    end record;

    type Pixel is record
        Color : Color_T;
        Char  : Character;
    end record;
    type Pix_Image_T is array (Natural range <>, Natural range <>) of Pixel;
    type PImage_T (H : Natural; W : Natural) is new Component_T with record
        Image : Pix_Image_T (1 .. H, 1 .. W);
    end record;

    type Line_Types_T is (Left, Right, Top, Bottom);
    type Box_Map_T is array (Line_Types_T) of Character;
    type Box_T is new Component_T with record
        Box_Map : Box_Map_T;
        H, W    : Natural;
    end record;

    type Int_Tuple_T is array (1 .. 2) of Integer;
    function From_O (O : Origin_T; H : Natural; W : Natural) return Int_Tuple_T is
    begin
        case O is
            when Top_Left     => return (0,       0);
            when Bottom_Left  => return (-H,      0);
            when Center       => return (-H/2, -W/2);
            when Bottom_Right => return (-H,     -W);
            when Top_Right    => return (0,      -W);
        end case;
    end;

    type Proc_Line_Param_T is array (Natural range <>) of Natural;
    type Proc_Image_T (Count : Natural) is new Component_T with record
        Leading_Spaces : Proc_Line_Param_T (1 .. Count);
        Periods        : Proc_Line_Param_T (1 .. Count);
    end record;
    
    type Mover_T is new System_T with null record;
    procedure Execute (Self : Mover_T;
                       Dt   : Duration; 
                       E    : in out Entity_T; 
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        T   renames Transform_T        (E.Get_Components (Transform_T'Tag)(0).all);
        B   renames Body_T             (E.Get_Components (Body_T'Tag)(0).all);
        S   renames Shuttle_Controls_T (E.Get_Components (Shuttle_Controls_T'Tag)(0).all);
        FB0 renames PImage_T           (ES(0).Get_Components (PImage_T'Tag)(0).all);
        Step : Float := Float (Dt);
        FX   : Float := S.Sequence (S.Idx).Thrust_X;
        FY   : Float := S.Sequence (S.Idx).Thrust_Y;
        TopY : Float := Float (FB0.Image'Last (1));
    begin
        T.X := B.X0 + B.VX0 * Step + FX / (2.0 * B.Mass) * Step**2;
        T.Y := TopY - (B.Y0 + B.VY0 * Step + FY / (2.0 * B.Mass) * Step**2 - 0.5 * 9.8 * Step**2);
    end;

    type Drawer_T is new System_T with null record;
    procedure Execute (Self : Drawer_T;
                       Dt   : Duration; 
                       E    : in out Entity_T; 
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        Imgs  :           Components_T := E.Get_Components (Image_T'Tag);
        Trans :           Components_T := E.Get_Components (Transform_T'Tag);
        Color_Map renames Color_Map_T    (E.Get_Components (Color_Map_T'Tag)(0).all);
        FB0       renames PImage_T       (ES(0).Get_Components (PImage_T'Tag)(0).all);

        function Within_Y (Y : Integer) return Boolean is 
            (Y >= FB0.Image'First (1) and Y <= FB0.Image'Last (1));
        function Within_X (X : Integer) return Boolean is 
            (X >= FB0.Image'First (2) and X <= FB0.Image'Last (2));
    begin
        for I in Imgs'Range loop
            declare
                Img renames Image_T (Imgs (I).all);
                T renames Transform_T (Trans (I).all);
            begin
                for R in Img.Image'Range(1) loop
                    for C in Img.Image'Range(2) loop
                        declare
                            Char   : Character   := Img.Image (R, C);
                            Offset : Int_Tuple_T := From_O (Img.O, Img.H, Img.W);
                            Y      : Integer     := Integer (T.Y) + Offset (1) + R;
                            X      : Integer     := Integer (T.X) + Offset (2) + C;
                        begin
                            if Char /= ',' and Within_Y (Y) and Within_X (X) then
                                FB0.Image (Y, X).Char := Char;
                                FB0.Image (Y, X).Color := Colors (Color_Map.CM (Char));
                            end if;
                        end;
                    end loop;
                end loop;
            end;
        end loop;
    end;

    Drawer_Sys : aliased Drawer_T;

    type Grader_T is new System_T with null record;
    procedure Execute (Self : Grader_T;
                       Dt   : Duration; 
                       E    : in out Entity_T; 
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        T   renames Transform_T  (E.Get_Components (Transform_T'Tag)(0).all);
        CM  renames Color_Map_T  (E.Get_Components (Color_Map_T'Tag)(0).all);
        PRI renames Proc_Image_T (E.Get_Components (Proc_Image_T'Tag)(0).all);
        FB0 renames PImage_T     (ES(0).Get_Components (PImage_T'Tag)(0).all);

        function Line (LS : Natural; P : Natural; W: Natural) return Char_Image_T is
            use Ada.Strings.Fixed;
            CI : Char_Image_T (1 .. 1, 1 .. W) := (others => (others => ' '));
        begin
            if P /= 0 then
                declare
                    Seq : String := (LS * " ") & ((W + P) / P) * ("." & ((P - 1) * " "));
                begin
                    for I in CI'Range (2) loop
                        CI (1, I) := Seq (I);
                    end loop;
                end;
            end if;
            return CI;
        end;
    begin
        for I in 1 .. PRI.Count loop
            declare
                L : Char_Image_T := Line (PRI.Leading_Spaces (I), PRI.Periods (I), FB0.W);
                Gradient_Line : Entity_T := (2,
                                             Id         => "GradL",
                                             Components => (0 => new Transform_T' (X => 0.0, Y => Float (I)),
                                                            1 => new Image_T' (1, FB0.W, L, Bottom_Left),
                                                            2 => CM'Access));
            begin
                Execute (Drawer_Sys, Dt, Gradient_Line, ES);
            end;
        end loop;
    end;

    type Animator_T is new System_T with null record;
    procedure Execute (Self : Animator_T;
                       Dt   : Duration; 
                       E    : in out Entity_T; 
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        SC renames Shuttle_Controls_T (E.Get_Components (Shuttle_Controls_T'Tag)(0).all);
    begin
        if SC.Idx /= Sequence_Idx_T'First and SC.Idx /= Sequence_Idx_T'Last then
            declare
                A renames Animation_T (E.Get_Components (Animation_T'Tag)(0).all);
                T renames Transform_T (E.Get_Components (Transform_T'Tag)(0).all);
                EA : Entity_T := (2,
                                  Id         => "Shu_A", 
                                  Components => (0 => new Transform_T'(X => T.X, Y => T.Y),
                                                 1 => Component_Access (A.Frames (A.Current)), 
                                                 2 => new Color_Map_T'(CM => Colors_Map_T'('*'    => Yellow,
                                                                                            others => Gray))));
            begin
                Execute (Drawer_Sys, Dt, EA, ES);
                A.Current := A.Current + 1;
            end;
        end if;
    end;

    type Star_Randomizer_T is new System_T with null record;
    procedure Execute (Self : Star_Randomizer_T;
                       Dt   : Duration; 
                       E    : in out Entity_T; 
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        Imgs :      Components_T :=     E.Get_Components (Image_T'Tag);
        Maps :      Components_T :=     E.Get_Components (Color_Map_T'Tag);
        SR  renames Random_Stars_T     (E.Get_Components (Random_Stars_T'Tag)(0).all);
        FB0 renames PImage_T           (ES (0).Get_Components (PImage_T'Tag)(0).all);
        SC  renames Shuttle_Controls_T (ES (1).Get_Components (Shuttle_Controls_T'Tag)(0).all);

        function Random (Min : Natural; Max : Natural) return Natural is
            subtype R is Natural range Min .. Max;
            package Rand is new Ada.Numerics.Discrete_Random (R);
            Gen : Rand.Generator;
        begin
            Rand.Reset (Gen);
            return Rand.Random (Gen);
        end;
    begin
        if SC.Idx = Sequence_Idx_T'First then
            for I in 1 .. SR.I'Length loop
                declare
                    Idx : Natural := Random (0, Imgs'Length - 1);
                    RX  : Natural := Random (FB0.Image'First (2), FB0.Image'Last (2));
                    RY  : Natural := Random (FB0.Image'First (1), FB0.Image'Last (1));
                begin
                    SR.I (I) := Idx;
                    SR.T (I) := (Float (RX), float (RY));
                end;
            end loop;
        end if;
        for S in 1 .. SR.Count loop
            declare
                Star : Entity_T := (2,
                                    Id         => "StarX",
                                    Components => (0 => new Transform_T' (SR.T (S)),
                                                   1 => Component_Access (Imgs (SR.I (S))),
                                                   2 => Component_Access (Maps (SR.I (S)))));
            begin
                Execute (Drawer_Sys, Dt, Star, ES);
            end;
        end loop;
    end;

    type Boxer_T is new System_T with null record;
    procedure Execute (Self : Boxer_T;
                       Dt   : Duration; 
                       E    : in out Entity_T; 
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        T    renames Transform_T (E.Get_Components (Transform_T'Tag)(0).all);
        Box  renames Box_T       (E.Get_Components (Box_T'Tag)(0).all);
        CMap renames Color_Map_T (E.Get_Components (Color_Map_T'Tag)(0).all); 
        FB0  renames PImage_T    (ES(0).Get_Components (PImage_T'Tag)(0).all);

        use Ada.Strings.Fixed;

        procedure Draw (LT : Line_Types_T; T : Transform_T; Line: String; B: Box_T) is
            Row : Integer := Integer (T.Y);
            Col : Integer := Integer (T.X);
        begin
            for I in Line'Range loop
                FB0.Image (Row + (if LT = Top or LT = Bottom then 1 else I), 
                           Col + (if LT = Top or LT = Bottom then I else 1)).Char := Line (I);
                FB0.Image (Row + (if LT = Top or LT = Bottom then 1 else I), 
                           Col + (if LT = Top or LT = Bottom then I else 1)).Color := Colors (CMap.CM (B.Box_Map (LT)));
            end loop;
        end;

    begin
        for LT in Box.Box_Map'Range loop
            case LT is
                when Top => 
                    Draw (LT, T, Box.W * Box.Box_Map (Top), Box);
                when Bottom => 
                    Draw (LT, (T.X, T.Y + Float (Box.H) - 1.0), Box.W * Box.Box_Map (Bottom), Box);
                when Left => 
                    Draw (LT, T, Box.H * Box.Box_Map (Left), Box);
                when Right => 
                    Draw (LT, (T.X + Float (Box.W) - 1.0, T.Y), Box.H * Box.Box_Map (Right), Box);
            end case;
        end loop;
    end;

    type Titler_T is new System_T with null record;
    procedure Execute (Self : Titler_T;
                       Dt   : Duration; 
                       E    : in out Entity_T; 
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        T    renames Transform_T (E.Get_Components (Transform_T'Tag)(0).all);
        Img  renames Image_T     (E.Get_Components (Image_T'Tag)(0).all);
        CMap renames Color_Map_T (E.Get_Components (Color_Map_T'Tag)(0).all); 
        FB0   renames PImage_T   (ES(0).Get_Components (PImage_T'Tag)(0).all);
        Offset : Int_Tuple_T := From_O (Img.O, Img.H, Img.W);
    begin
        for R in Img.Image'Range (1) loop
            for C in Img.Image'Range (2) loop
                FB0.Image (Integer (T.Y) + Offset (1) + R, 
                           Integer (T.X) + Offset (2) + C).Char := Img.Image (R, C);
                FB0.Image (Integer (T.Y) + Offset (1) + R, 
                           Integer (T.X) + Offset (2) + C).Color := Colors (CMap.CM (Img.Image (R, C)));
            end loop;
        end loop;
    end;

    function Trim (S : String) return String is
            (S (S'First + 1 .. S'Last));

    function Move_Cursor (R : Natural; C : Natural) return String is
        (ESC & "[" & Trim (R'Image) & ";" & Trim (C'Image) & "H");

    type Renderer_T is new System_T with null record;
    procedure Execute (Self : Renderer_T;
                       Dt   : Duration; 
                       E    : in out Entity_T; 
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        FB0 renames PImage_T (E.Get_Components (PImage_T'Tag)(0).all);
        FB1 renames PImage_T (E.Get_Components (PImage_T'Tag)(1).all);

        function Hide_Cursor return String is (ESC & "[?25l");
        function Text_Color  return String is (ESC & "[38;2");
        function Reset       return String is (ESC & "[0m");
        function Terminator  return String is ("m"); 

        function C_To_S (C : Channel) return String is
            type U8 is mod 256;
            CU8 : U8 with address => C'Address;
            Img : String := CU8'Image;
        begin
            return Img (2 .. Img'Length);
        end;

        function C_To_S (C : Color_T) return String is
            (";" & C_To_S (C.R) & ";" & C_To_S (C.G) & ";" & C_To_S (C.B));
    begin
        Put (Hide_Cursor);
        for R in FB0.Image'Range(1) loop
            for C in FB0.Image'Range(2) loop
                if FB0.Image (R, C).Char /= FB1.Image (R, C).Char or
                   FB0.Image (R, C).color /= FB1.Image (R, C).Color then
                    Put (Move_Cursor (R, C));
                    Put (Text_Color & C_To_S (FB0.Image (R, C).Color) & Terminator & FB0.Image (R, C).Char & Reset);
                end if; 
            end loop;
        end loop;
        FB1.Image := FB0.Image;
        FB0.Image := (others => (others => (Colors (Gray), '.')));
    end;

    Width  : constant := 124;
    Height : constant := 36;

    Shuttle : aliased Entity_T := 
                (5,
                 Id => "Shutl", 
                 Components => (0 => new Transform_T'(X => 0.0, Y => 0.0),
                                1 => new Body_T,
                                2 => new Image_T'(4, 5, (",,|,,",
                                                         ",/ \,",
                                                         ",|o|,",
                                                         "/- -\"), Bottom_Left), 
                                3 => new Animation_T'(Frames  => Frames_T'(0 => new Image_T'(2, 5, (" * * ",
                                                                                                    "* * *"), Top_Left),
                                                                           1 => new Image_T'(2, 5, ("* * *",
                                                                                                   " * * "), Top_Left)),
                                                      Current => Anim_Count_T'First),
                                4 => new Color_Map_T'(CM => Colors_Map_T'('|' | '/' | '\' | '-' => Magenta,
                                                                          'o'                   => Green,
                                                                          '*'                   => Yellow,
                                                                          others                => Gray)),
                                5 => new Shuttle_Controls_T'(Sequence => (others => (0.0, 19450000.0)),
                                                             Idx      => Sequence_Idx_T'First)));
    Horizon : Entity_T := 
            (2,
             Id => "Hrzon",
             Components => (0 => new Transform_T'(X => 0.0, Y => 35.0),
                            1 => new Image_T'(5, Width, 
                           (",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                                     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
                            ",,,,,,,,,,,,,,,,,,,,,,                  ___-_-===========-==-----__                              ,,,,,,,,,,,,,,,,,,,,,,,,,,,",
                            ",,,,,,,,,              ___---=======---                             --=--====-=========_____                    ,,,,,,,,,,,,",
                            ",,      ______--- -_--                                                                        -- _-------- ___ ___ _       ,",
                            "____---                                                                                                              - - -__"),
                            Bottom_Left
                            ),
                            2 => new Color_Map_T'(CM => Colors_Map_T'('_' | '-' | '=' => White,
                                                                      others          => Gray))));
    Saturn : Entity_T := (2,
                          Id => "Satrn",
                          Components => (0 => new Transform_T'(X => 50.0, Y => 10.0),
                                         1 => new Image_T'(5, 15, (".-..='``'=.,,,,",
                                                                   "'=/_       \,,,",
                                                                   ",|  '=._    |,,",
                                                                   ",,\     `=./`.,",
                                                                   ",,,'=.__.=',`='"), Center),
                                         2 => new Color_Map_T'(CM => Colors_Map_T'('.' | ''' | '\' | '|' | '`' => Orange,
                                                                                   '-' | '_'                   => Blue,
                                                                                   '='                         => Yellow,
                                                                                   others                      => Gray))));
    Stars : Entity_T := (10,
                          Id => "Stars",
                          Components => (0  => new Image_T'(3, 5, (",,|,,",
                                                                   "- o -",
                                                                   ",,|,,"), Center),
                                         1  => new Color_Map_T'(CM => Colors_Map_T'(others => Gray)),
                                         2  => new Image_T'(1, 2, (0 => "()"), Center),
                                         3  => new Color_Map_T'(CM => Colors_Map_T'(others => Gray)),
                                         4  => new Image_T'(1, 1, (0 => "O"), Center),
                                         5  => new Color_Map_T'(CM => Colors_Map_T'('O'    => Light_Gray,
                                                                                    others => Gray)),
                                         6  => new Image_T'(1, 1, (0 => "+"), Center),
                                         7  => new Color_Map_T'(CM => Colors_Map_T'(others => White)),
                                         8  => new Image_T'(1, 1, (0 => "*"), Center),
                                         9  => new Color_Map_T'(CM => Colors_Map_T'(others => White)),
                                         10 => new Random_Stars_T'(Count => 5 * 16, 
                                                                   I     => (others => 1),
                                                                   T     => (others => (X => 0.0, Y => 0.0)))));
    Window_Box : Entity_T := (2,
                              Id         => "WndwB",
                              Components => (0 => new Transform_T'(X => 0.0, Y => 0.0),
                                             1 => new Box_T'(Box_Map => (Top => '_', Bottom => ' ', Left => '|', Right => '|'),
                                                             H       => Height, 
                                                             W       => Width),
                                             2 => new Color_Map_T'(CM => Colors_Map_T'('|' | '_' => White,
                                                                                       others    => Gray))));
    Title : Entity_T := (2, Id => "Title",
                                          Components => (0 => new Transform_T'(X => Float (Width) - 5.0, Y => 0.0),
                                                         1 => new Image_T'(1, 21, (0 => "/ GNAT SAS TUTORIAL /"), Top_Right),
                                                         2 => new Color_Map_T'(CM => Colors_Map_T'('/'    => White,
                                                                                                   others => Pink))));
    Frame_Buffer : aliased Entity_T := (1,
                                        Id => "Frbuf",
                                        Components => (0 => new PImage_T'(Height, Width, (others => (others => (Colors (Gray), '.')))),
                                                       1 => new PImage_T'(Height, Width, (others => (others => (Colors (Gray), ','))))));
    Bgd_Gradient : Entity_T := (2,
                                Id         => "BgdGr",
                                Components => (0 => new Transform_T'(X => 0.0, Y => 0.0),
                                               1 => new Proc_Image_T' (
                                                    Count => 36,
                                                    Leading_Spaces => (0,0,0,0,4,0,0,0,0,0,0,2,0,0,0,0,0,2,0,0,0,2,0,0,0,2,0,1,0,0,0,0,0,0,0,0),
                                                    Periods        => (8,0,0,0,8,0,0,0,4,0,0,4,0,0,4,0,0,4,0,4,0,4,0,4,0,4,2,2,1,1,1,1,1,1,1,1)),
                                               2 => new Color_Map_T'(CM => Colors_Map_T'(others => Gray))));

    Mover_Sys           : Mover_T;
    Grader_Sys          : Grader_T;
    Animator_Sys        : Animator_T;
    Star_Sys            : Star_Randomizer_T;
    Boxer_Sys           : Boxer_T;
    Titler_Sys          : Titler_T;
    Renderer_Sys        : Renderer_T;

    Start : Time := Clock;
begin
    declare
        S   renames Shuttle_Controls_T (Shuttle.Get_Components (Shuttle_Controls_T'Tag)(0).all);
        FB0 renames PImage_T           (Frame_Buffer.Get_Components (PImage_T'Tag)(0).all);
    begin
        for Idx in Sequence_Idx_T'Range loop
            S.Idx := Idx;
            Execute (Grader_Sys,   Duration(Idx), Bgd_Gradient, Entities_T'(0 => Frame_Buffer'Access));
            Execute (Mover_Sys,    Duration(Idx), Shuttle,      Entities_T'(0 => Frame_Buffer'Access));
            Execute (Star_Sys,     Duration(Idx), Stars,        Entities_T'(0 => Frame_Buffer'Access, 1 => Shuttle'Access));
            Execute (Drawer_Sys,   Duration(Idx), Horizon,      Entities_T'(0 => Frame_Buffer'Access));
            Execute (Drawer_Sys,   Duration(Idx), Saturn,       Entities_T'(0 => Frame_Buffer'Access));
            Execute (Drawer_Sys,   Duration(Idx), Shuttle,      Entities_T'(0 => Frame_Buffer'Access));
            Execute (Animator_Sys, Duration(Idx), Shuttle,      Entities_T'(0 => Frame_Buffer'Access));
            Execute (Boxer_Sys,    Duration(Idx), Window_Box,   Entities_T'(0 => Frame_Buffer'Access));
            Execute (Titler_Sys,   Duration(Idx), Title,        Entities_T'(0 => Frame_Buffer'Access));
            Execute (Renderer_Sys, Duration(Idx), Frame_Buffer);
            delay 0.15;
        end loop;
        Put (Move_Cursor (FB0.Image'Last (1), FB0.Image'Last (2)));
        New_Line;
    end;
end Simulator;