VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Mainform 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Maze Run Level Editor - Untitled Levels"
   ClientHeight    =   8655
   ClientLeft      =   150
   ClientTop       =   720
   ClientWidth     =   9750
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   577
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   650
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox Picture1 
      BackColor       =   &H000000FF&
      BorderStyle     =   0  'None
      Height          =   330
      Left            =   4050
      ScaleHeight     =   22
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   2
      TabIndex        =   29
      Top             =   6600
      Width           =   30
   End
   Begin VB.CommandButton cmd_Swapforward 
      Caption         =   "Swap Forward"
      Height          =   495
      Left            =   5040
      TabIndex        =   28
      Top             =   7800
      Width           =   735
   End
   Begin VB.CommandButton cmd_Swapback 
      Caption         =   "Swap Back"
      Height          =   495
      Left            =   4320
      TabIndex        =   27
      Top             =   7800
      Width           =   735
   End
   Begin MSComDlg.CommonDialog CommonDialog 
      Left            =   5040
      Top             =   5760
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
      Filter          =   "Binary Files (.bin)|*.bin"
   End
   Begin VB.CommandButton cmd_Delete 
      Caption         =   "Delete Current"
      Height          =   495
      Left            =   3600
      TabIndex        =   24
      Top             =   7800
      Width           =   735
   End
   Begin VB.CommandButton cmd_InsertAfter 
      Caption         =   "Insert After"
      Height          =   495
      Left            =   2880
      TabIndex        =   23
      Top             =   7800
      Width           =   735
   End
   Begin VB.CommandButton cmd_InsertBefore 
      Caption         =   "Insert Before"
      Height          =   495
      Left            =   2160
      TabIndex        =   22
      Top             =   7800
      Width           =   735
   End
   Begin VB.CommandButton cmd_Last 
      Caption         =   ">|"
      Height          =   495
      Left            =   1560
      TabIndex        =   21
      Top             =   7800
      Width           =   495
   End
   Begin VB.CommandButton cmd_Next 
      Caption         =   ">"
      Height          =   495
      Left            =   1080
      TabIndex        =   20
      Top             =   7800
      Width           =   495
   End
   Begin VB.CommandButton cmd_Previous 
      Caption         =   "<"
      Height          =   495
      Left            =   600
      TabIndex        =   19
      Top             =   7800
      Width           =   495
   End
   Begin VB.CommandButton cmd_First 
      Caption         =   "|<"
      Height          =   495
      Left            =   120
      TabIndex        =   18
      Top             =   7800
      Width           =   495
   End
   Begin VB.CommandButton ChangeCol 
      Caption         =   "Change Background"
      Height          =   495
      Left            =   720
      TabIndex        =   17
      Top             =   6960
      Width           =   1095
   End
   Begin VB.PictureBox pic_Cur 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000001&
      BorderStyle     =   0  'None
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   480
      Left            =   5280
      ScaleHeight     =   32
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   32
      TabIndex        =   14
      Top             =   7080
      Width           =   480
   End
   Begin VB.PictureBox MapBox 
      Appearance      =   0  'Flat
      BackColor       =   &H80000001&
      BorderStyle     =   0  'None
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   5760
      Left            =   0
      ScaleHeight     =   384
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   384
      TabIndex        =   13
      Top             =   0
      Width           =   5760
      Begin VB.Label Lbl_TDest 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00C0E0FF&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "T1"
         ForeColor       =   &H80000008&
         Height          =   240
         Index           =   1
         Left            =   0
         TabIndex        =   30
         Top             =   0
         Visible         =   0   'False
         Width           =   240
      End
      Begin VB.Label Lbl_TDest 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00C0E0FF&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "T2"
         ForeColor       =   &H80000008&
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   31
         Top             =   0
         Visible         =   0   'False
         Width           =   240
      End
      Begin VB.Label Lbl_TDest 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00C0E0FF&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "T3"
         ForeColor       =   &H80000008&
         Height          =   240
         Index           =   3
         Left            =   240
         TabIndex        =   32
         Top             =   0
         Visible         =   0   'False
         Width           =   240
      End
      Begin VB.Label Lbl_TDest 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00C0E0FF&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "T4"
         ForeColor       =   &H80000008&
         Height          =   240
         Index           =   4
         Left            =   0
         TabIndex        =   33
         Top             =   240
         Visible         =   0   'False
         Width           =   240
      End
      Begin VB.Label Lbl_TDest 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00C0E0FF&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "T5"
         ForeColor       =   &H80000008&
         Height          =   240
         Index           =   5
         Left            =   120
         TabIndex        =   34
         Top             =   240
         Visible         =   0   'False
         Width           =   240
      End
      Begin VB.Label Lbl_TDest 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00C0E0FF&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "T6"
         ForeColor       =   &H80000008&
         Height          =   240
         Index           =   6
         Left            =   240
         TabIndex        =   35
         Top             =   240
         Visible         =   0   'False
         Width           =   240
      End
   End
   Begin VB.TextBox tx_Gems 
      Height          =   285
      Left            =   3120
      TabIndex        =   11
      Top             =   7320
      Width           =   855
   End
   Begin VB.CommandButton cmd_Teleport 
      Caption         =   "Teleport 6"
      Height          =   375
      Index           =   6
      Left            =   4845
      TabIndex        =   8
      Top             =   6120
      Width           =   900
   End
   Begin VB.CommandButton cmd_Teleport 
      Caption         =   "Teleport 5"
      Height          =   375
      Index           =   5
      Left            =   3960
      TabIndex        =   7
      Top             =   6120
      Width           =   900
   End
   Begin VB.CommandButton cmd_Teleport 
      Caption         =   "Teleport 4"
      Height          =   375
      Index           =   4
      Left            =   3000
      TabIndex        =   6
      Top             =   6120
      Width           =   975
   End
   Begin VB.CommandButton cmd_Teleport 
      Caption         =   "Teleport 3"
      Height          =   375
      Index           =   3
      Left            =   2040
      TabIndex        =   5
      Top             =   6120
      Width           =   975
   End
   Begin VB.CommandButton cmd_Teleport 
      Caption         =   "Teleport 2"
      Height          =   375
      Index           =   2
      Left            =   1080
      TabIndex        =   4
      Top             =   6120
      Width           =   975
   End
   Begin VB.CommandButton cmd_Teleport 
      Caption         =   "Teleport 1"
      Height          =   375
      Index           =   1
      Left            =   120
      TabIndex        =   3
      Top             =   6120
      Width           =   975
   End
   Begin VB.TextBox tx_Password 
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   2760
      MaxLength       =   10
      TabIndex        =   2
      Text            =   "1234567890"
      Top             =   6960
      Width           =   1215
   End
   Begin VB.TextBox tx_Name 
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      Left            =   2760
      MaxLength       =   24
      TabIndex        =   1
      Text            =   "123456789012345678901234"
      Top             =   6600
      Width           =   2610
   End
   Begin VB.PictureBox ItemPalette 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   8655
      Left            =   5880
      Picture         =   "Mainform.frx":0000
      ScaleHeight     =   577
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   257
      TabIndex        =   0
      Top             =   0
      Width           =   3855
   End
   Begin VB.Label Label6 
      Alignment       =   2  'Center
      Caption         =   "Current Tile:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   4320
      TabIndex        =   26
      Top             =   7080
      Width           =   975
   End
   Begin VB.Label lbl_LevNo 
      Alignment       =   2  'Center
      BorderStyle     =   1  'Fixed Single
      Caption         =   "Level 1 of 1"
      Height          =   255
      Left            =   120
      TabIndex        =   25
      Top             =   8400
      Width           =   5655
   End
   Begin VB.Line Line1 
      X1              =   8
      X2              =   384
      Y1              =   512
      Y2              =   512
   End
   Begin VB.Label Label5 
      Caption         =   "Password"
      Height          =   255
      Left            =   2040
      TabIndex        =   16
      Top             =   6960
      Width           =   855
   End
   Begin VB.Label Label4 
      Caption         =   "Name"
      Height          =   255
      Left            =   2280
      TabIndex        =   15
      Top             =   6600
      Width           =   615
   End
   Begin VB.Shape shp_BackCol 
      BackColor       =   &H00000000&
      BackStyle       =   1  'Opaque
      BorderStyle     =   0  'Transparent
      Height          =   495
      Left            =   120
      Top             =   6960
      Width           =   495
   End
   Begin VB.Label Label3 
      Caption         =   "Background Colour"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   12
      Top             =   6600
      Width           =   1575
   End
   Begin VB.Label Label2 
      Caption         =   "Gems Needed"
      Height          =   255
      Left            =   2040
      TabIndex        =   10
      Top             =   7320
      Width           =   1215
   End
   Begin VB.Label Label1 
      Caption         =   "Place Teleport Destinations:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   9
      Top             =   5880
      Width           =   2175
   End
   Begin VB.Menu mnu_file 
      Caption         =   "File"
      Begin VB.Menu mnu_new 
         Caption         =   "New"
      End
      Begin VB.Menu mnu_load 
         Caption         =   "Load"
      End
      Begin VB.Menu mnu_save 
         Caption         =   "Save"
      End
      Begin VB.Menu mnu_saveas 
         Caption         =   "Save As"
      End
   End
   Begin VB.Menu mnu_Tools 
      Caption         =   "Tools"
      Begin VB.Menu mnu_Clear 
         Caption         =   "Clear Level"
      End
      Begin VB.Menu mnu_AddBorder 
         Caption         =   "Add Border"
      End
   End
   Begin VB.Menu mnu_Test 
      Caption         =   "Test"
      Begin VB.Menu mnu_TestLevel 
         Caption         =   "Test Level"
      End
      Begin VB.Menu mnu_s 
         Caption         =   "-"
      End
      Begin VB.Menu mnu_EmulatorPath 
         Caption         =   "Emulator Path"
      End
   End
End
Attribute VB_Name = "Mainform"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim Levels As Collection
Dim Level As LevelClass
Dim Tiles(255) As ImageType
Dim cTile As Long
Dim SaveFile As String
Dim Mode As Byte

Dim EmulatorPath As String

Const PAPER_BLACK = 0
Const PAPER_BLUE = 8
Const PAPER_RED = 16
Const BRIGHT = 64

Private Sub ChangeCol_Click()
    Select Case Level.BackCol
    Case PAPER_BLACK
        Level.BackCol = PAPER_RED
        shp_BackCol.BackColor = RGB(255, 0, 0)
    Case PAPER_RED
        Level.BackCol = PAPER_BLUE
        shp_BackCol.BackColor = RGB(0, 0, 255)
    Case PAPER_BLUE
        Level.BackCol = PAPER_BLACK
        shp_BackCol.BackColor = RGB(0, 0, 0)
    End Select
    RedrawMap
End Sub

Private Sub cmd_InsertAfter_Click()
Dim Index As Long

    UpdateLevelObject
    Index = GetLevelIndex()
    Set Level = New LevelClass
    Levels.Add Level, , , Index
    UpdateLevelInfo

End Sub

Private Sub cmd_Delete_Click()
Dim Index As Long

    If Levels.Count > 1 Then
        If MsgBox("Sure?", vbOKCancel) = vbCancel Then Exit Sub
        Index = GetLevelIndex()
        If Index > 1 Then
            Set Level = Levels.Item(Index - 1)
        Else
            Set Level = Levels.Item(2)
        End If
        Levels.Remove Index
        UpdateLevelInfo
    Else
        MsgBox "Must have at least 1 level!"
    End If
    
End Sub

Private Sub cmd_First_Click()

    UpdateLevelObject
    Set Level = Levels.Item(1)
    UpdateLevelInfo

End Sub

Private Sub cmd_InsertBefore_Click()
Dim Index As Long

    UpdateLevelObject
    Index = GetLevelIndex()
    Set Level = New LevelClass
    Levels.Add Level, , Index
    UpdateLevelInfo

End Sub

Private Sub cmd_Last_Click()
    
    UpdateLevelObject
    Set Level = Levels.Item(Levels.Count)
    UpdateLevelInfo

End Sub

Private Sub cmd_Next_Click()
Dim Index As Long

    Index = GetLevelIndex()
    If Index < Levels.Count Then
        UpdateLevelObject
        Set Level = Levels.Item(Index + 1)
        UpdateLevelInfo
    End If

End Sub

Function GetLevelIndex() As Long
Dim l As LevelClass, Index As Long

    Index = 1
    For Each l In Levels
        If l Is Level Then
            GetLevelIndex = Index
            Exit Function
        End If
        Index = Index + 1
    Next

End Function

Private Sub cmd_Previous_Click()
Dim Index As Long

    Index = GetLevelIndex()
    If Index > 1 Then
        UpdateLevelObject
        Set Level = Levels.Item(Index - 1)
        UpdateLevelInfo
    End If

End Sub

Private Sub cmd_Swapback_Click()
Dim Index As Long

    Index = GetLevelIndex()
    If Index > 1 Then
        Levels.Remove Index
        Levels.Add Level, , Index - 1
        UpdateLevelInfo
    End If

End Sub

Private Sub cmd_Swapforward_Click()
Dim Index As Long

    Index = GetLevelIndex()
    If Index < Levels.Count Then
        Levels.Remove Index
        Levels.Add Level, , , Index
        UpdateLevelInfo
    End If
End Sub

Private Sub cmd_Teleport_Click(Index As Integer)

    Mode = Index

End Sub

Private Sub Form_Load()

    Set Levels = New Collection
    Set Level = New LevelClass
    Levels.Add Level
    
    ItemPalette.Picture = LoadPicture(App.Path & "\Palette.bmp")
    Mainform.Show
    
    WindowGraphics MapBox ', 384, 384
    ResizeGraphics
    'PaletteImage = LoadImage(App.Path & "\Palette.bmp")
    LoadGraphics
    
    UpdateLevelInfo
    
    EmulatorPath = GetEmulatorPath()

End Sub

Function GetEmulatorPath() As String
Dim s As String

    If FileExists(App.Path & "\EmuPath.ini") = True Then
        Open App.Path & "\EmuPath.ini" For Input As #1
        Line Input #1, s
        s = Trim(s)
        GetEmulatorPath = s
        Close #1
        Exit Function
    End If

    GetEmulatorPath = ""

End Function

Sub WriteEmulatorPath(s As String)

    If FileExists(App.Path & "\EmuPath.ini") = True Then
        Kill App.Path & "\EmuPath.ini"
    End If

    Open App.Path & "\EmuPath.ini" For Output As #1
    Seek #1, 1
    s = Trim(s)
    Print #1, , s
    Close #1

End Sub

Sub UpdateLevelInfo()
Dim Index As Long

    tx_Name = Level.Name
    tx_Password = Level.Password
    tx_Gems = Level.GemsNeeded
        
    Select Case Level.BackCol
    Case PAPER_BLACK
        shp_BackCol.BackColor = RGB(0, 0, 0)
    Case PAPER_BLUE
        shp_BackCol.BackColor = RGB(0, 0, 255)
    Case PAPER_RED
        shp_BackCol.BackColor = RGB(255, 0, 0)
    End Select
    
    Index = GetLevelIndex()
    lbl_LevNo.Caption = "Level " & Index & " of " & Levels.Count
    RedrawMap
    
End Sub

Sub UpdateLevelObject()
Dim g As Long

    Level.Name = UCase(tx_Name.Text)
    Level.Password = UCase(tx_Password.Text)
    
    g = Int(Val(tx_Gems.Text))
    If g > 99 Then g = 99
    If g < 0 Then g = 0
    Level.GemsNeeded = g

End Sub

Sub LoadGraphics()
Dim a As Long
Dim PaletteImage As ImageType

    PaletteImage = LoadImage(App.Path & "\Palette.bmp")
    SetBuffer ImageBuffer(PaletteImage)
    
    For a = 0 To 255
        Tiles(a) = CreateImage(32, 32)
    Next
    
    For a = 0 To 143
        GrabImage Tiles(a), (a Mod 8) * 32, (a And 248) * 4
        MaskImage Tiles(a), RGB(0, 0, 0)
    Next a
    FreeImage PaletteImage
        
End Sub


Private Sub ItemPalette_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim tX As Long, tY As Long, a As Long

    If Button = vbLeftButton Then
    
        a = Int(Y / 32) * 8 + Int(X / 32)
        
        DrawToDC Tiles(a), pic_Cur.hdc, 0, 0
        pic_Cur.Picture = pic_Cur.Image
        pic_Cur.Refresh
        
        cTile = a
        
    End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
    EndGraphics
End Sub

Private Sub MapBox_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    MapBox_MouseMove Button, Shift, X, Y
End Sub

Private Sub MapBox_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim mX As Long, mY As Long

    If Mode = 0 Then
    
        If Button = vbLeftButton Then
            mX = Int(X / 32)
            mY = Int(Y / 32)
            
            If cTile <> 143 Then
                Level.SetTile mX, mY, cTile
            Else
                Level.SetTile mX, mY, 255
            End If
            RedrawMap
        ElseIf Button = vbRightButton Then
            mX = Int(X / 32)
            mY = Int(Y / 32)
            Level.SetTile mX, mY, 0
            RedrawMap
        End If
        
    Else
    
        If Button = vbLeftButton Then
            mX = Int(X / 32)
            mY = Int(Y / 32)
            Level.PutTeleport mX, mY, Mode - 1
            RedrawMap
            Mode = 0
        End If
    
    End If
End Sub

Private Sub MapBox_Paint()
    Flip
End Sub

Sub RedrawMap()
Dim X As Long, Y As Long, C As Long
    SetBuffer BackBuffer()
    
    Select Case Level.BackCol
    Case PAPER_BLACK
        ClsBuffer RGB(0, 0, 0)
    Case PAPER_RED
        ClsBuffer RGB(0, 0, 255)
    Case PAPER_BLUE
        ClsBuffer RGB(255, 0, 0)
    End Select
    
    For X = 0 To 11
        For Y = 0 To 11
            C = Level.GetTile(X, Y)
            If C <> 0 Then
                If C <> 255 Then
                    DrawImage Tiles(C), X * 32, Y * 32
                Else
                    DrawImage Tiles(143), X * 32, Y * 32
                End If
            End If
        Next
    Next
    
    Flip
    
    For C = 1 To 6
        If Level.GetTeleportX(C - 1) = 0 And Level.GetTeleportY(C - 1) = 0 Then
            Lbl_TDest(C).Visible = False
        Else
            Lbl_TDest(C).Left = Level.GetTeleportX(C - 1) * 32
            Lbl_TDest(C).Top = Level.GetTeleportY(C - 1) * 32
            If C >= 4 Then
                Lbl_TDest(C).Top = Lbl_TDest(C).Top + 16
            End If
            If C = 2 Or C = 5 Then
                Lbl_TDest(C).Left = Lbl_TDest(C).Left + 8
            End If
            If C = 3 Or C = 6 Then
                Lbl_TDest(C).Left = Lbl_TDest(C).Left + 16
            End If
            Lbl_TDest(C).Visible = True
        End If
    Next
    

    
End Sub

Private Sub mnu_AddBorder_Click()
Dim X As Long, Y As Long
    If MsgBox("Sure?", vbOKCancel) = vbCancel Then Exit Sub

    For X = 0 To 11
        Level.SetTile X, 0, cTile
        Level.SetTile X, 11, cTile
    Next
    
    For Y = 1 To 10
        Level.SetTile 0, Y, cTile
        Level.SetTile 11, Y, cTile
    Next
    UpdateLevelInfo

End Sub

Private Sub mnu_Clear_Click()
Dim X As Long, Y As Long

    If MsgBox("Sure?", vbOKCancel) = vbCancel Then Exit Sub
    For Y = 0 To 11
        For X = 0 To 11
            Level.SetTile X, Y, cTile
        Next
    Next
    UpdateLevelInfo

End Sub

Private Sub mnu_EmulatorPath_Click()
Dim s As String
    s = InputBox("Emulator Executable:", , GetEmulatorPath())
    If s = "" Then Exit Sub
    WriteEmulatorPath s
    EmulatorPath = s
End Sub

Private Sub mnu_load_Click()

    CommonDialog.ShowOpen
    If SaveFile <> "" Then
        If MsgBox("Sure?", vbOKCancel) = vbCancel Then Exit Sub
    End If
    SaveFile = CommonDialog.FileName
    LoadLevels

End Sub

Sub LoadLevels()
Dim X As Long, Y As Long, B As Byte, pos As Long, a As Long

    Set Level = Nothing
    Set Levels = Nothing
    Set Levels = New Collection
    
    Open SaveFile For Binary As #1
    
    While Seek(1) < LOF(1)
        Set Level = New LevelClass
        Levels.Add Level
        
        pos = Seek(1)
        For Y = 0 To 11
            For X = 0 To 11
                Get #1, , B
                Level.SetTile X, Y, CLng(B)
            Next
            Seek #1, Seek(1) + 4
        Next
        
        'Background
        Seek #1, pos + 12
        Get #1, , B
        Level.BackCol = B - BRIGHT
        
        'Gems
        Seek #1, pos + 15
        Get #1, , B
        Level.GemsNeeded = B
        
        'Password
        Seek #1, pos + 28
        For a = 1 To 10
            Get #1, , B
            If B <> 0 Then Level.Password = Level.Password & Chr(B) Else Exit For
            Seek #1, Seek(1) + 15
        Next
        
        'Level name
        Seek #1, pos + 13
        For a = 1 To 12
            Get #1, , B
            If B <> 0 Then Level.Name = Level.Name & Chr(B) Else Exit For
            Seek #1, Seek(1) + 15
        Next
        Seek #1, pos + 14
        For a = 13 To 24
            Get #1, , B
            If B <> 0 Then Level.Name = Level.Name & Chr(B) Else Exit For
            Seek #1, Seek(1) + 15
        Next
        
        'Teleports
        Seek #1, pos + 31
        For a = 0 To 5
            Get #1, , B
            Level.PutTeleportByte B, a
            Seek #1, Seek(1) + 15
        Next
        
        Seek #1, pos + 192
    Wend
    
    Close #1
    
    Set Level = Levels.Item(1)
    UpdateLevelInfo
    Mainform.Caption = "Maze Run Level Editor - '" & SaveFile & "'"
    
End Sub

Private Sub mnu_new_Click()

    If MsgBox("Sure?", vbOKCancel) = vbCancel Then Exit Sub

    Set Level = Nothing
    Set Levels = Nothing
    
    Set Levels = New Collection
    Set Level = New LevelClass
    Levels.Add Level
    
    UpdateLevelInfo
    Mainform.Caption = "MazeRunEditor - Untitled Levels"

End Sub

Private Sub mnu_save_Click()

    If SaveFile = "" Then
        mnu_saveas_Click
    Else
        If FileExists(CommonDialog.FileName) Then
            Kill CommonDialog.FileName
        End If
        SaveLevels
    End If

End Sub

Private Sub mnu_saveas_Click()

    CommonDialog.ShowSave
    If FileExists(CommonDialog.FileName) Then
        Kill CommonDialog.FileName
    End If
    SaveFile = CommonDialog.FileName
    SaveLevels

End Sub

Sub SaveLevels()
Dim l As LevelClass, X As Long, Y As Long, pos As Long, a As Long
Dim s As Byte

    Mainform.Caption = "Maze Run Level Editor - '" & SaveFile & "'"
    
    UpdateLevelObject
    
    Open SaveFile For Binary As #1
    
    For Each l In Levels
    
        pos = Seek(1)
    
        For Y = 0 To 11
            For X = 0 To 11
                Put #1, , l.GetTile(X, Y)
            Next
            s = 0
            Put #1, , s
            Put #1, , s
            Put #1, , s
            Put #1, , s
        Next
        
        'Background
        Seek #1, pos + 12
        Put #1, , CByte(l.BackCol + BRIGHT)
        
        'Gems
        Seek #1, pos + 15
        Put #1, , l.GemsNeeded
        
        'Password
        Seek #1, pos + 28 'Seek(1) + 13
        For a = 1 To 10
            If a <= Len(l.Password) Then
                s = Asc(Mid(l.Password, a, 1))
            Else
                s = 0
            End If
            Put #1, , s
            Seek #1, Seek(1) + 15
        Next
        
        'Level name
        Seek #1, pos + 13
        For a = 1 To 12
            If a <= Len(l.Name) Then
                s = Asc(Mid(l.Name, a, 1))
            Else
                s = Asc(" ")
            End If
            Put #1, , s
            Seek #1, Seek(1) + 15
        Next
        Seek #1, pos + 14
        For a = 13 To 24
            If a <= Len(l.Name) Then
                s = Asc(Mid(l.Name, a, 1))
            Else
                s = Asc(" ")
            End If
            Put #1, , s
            Seek #1, Seek(1) + 15
        Next
        
        'Teleports
        Seek #1, pos + 31
        For a = 0 To 5
            Put #1, , l.GetTeleportByte(a)
            Seek #1, Seek(1) + 15
        Next
        
        Seek #1, pos + 192
        
    Next
    
    Close #1

End Sub

Public Function FileExists(ByVal file As String) As Boolean
Dim fLen As Integer

    On Error Resume Next
    fLen = Len(Dir$(file))
    If Err Or fLen = 0 Then
        FileExists = False
    Else
        FileExists = True
    End If
    
End Function

Private Sub mnu_TestLevel_Click()
Dim pos As Long, X As Long, Y As Long, s As Byte, l As LevelClass, a As Long
    
    If EmulatorPath = "" Then
        MsgBox "Emulator Path not set up!"
        Exit Sub
    End If

    UpdateLevelObject
    
    Set l = Level
    
    Open App.Path & "\MazeRunTestLevel.sna" For Binary As #1
    
    Seek #1, 20660 + 27 + 1
    
    pos = Seek(1)
    
        For Y = 0 To 11
            For X = 0 To 11
                Put #1, , l.GetTile(X, Y)
            Next
            s = 0
            Put #1, , s
            Put #1, , s
            Put #1, , s
            Put #1, , s
        Next
        
        'Background
        Seek #1, pos + 12
        Put #1, , CByte(l.BackCol + BRIGHT)
        
        'Gems
        Seek #1, pos + 15
        Put #1, , l.GemsNeeded
        
        'Password
        Seek #1, pos + 28 'Seek(1) + 13
        For a = 1 To 10
            If a <= Len(l.Password) Then
                s = Asc(Mid(l.Password, a, 1))
            Else
                s = 0
            End If
            Put #1, , s
            Seek #1, Seek(1) + 15
        Next
        
        'Level name
        Seek #1, pos + 13
        For a = 1 To 12
            If a <= Len(l.Name) Then
                s = Asc(Mid(l.Name, a, 1))
            Else
                s = Asc(" ")
            End If
            Put #1, , s
            Seek #1, Seek(1) + 15
        Next
        Seek #1, pos + 14
        For a = 13 To 24
            If a <= Len(l.Name) Then
                s = Asc(Mid(l.Name, a, 1))
            Else
                s = Asc(" ")
            End If
            Put #1, , s
            Seek #1, Seek(1) + 15
        Next
        
        'Teleports
        Seek #1, pos + 31
        For a = 0 To 5
            Put #1, , l.GetTeleportByte(a)
            Seek #1, Seek(1) + 15
        Next
        
    Close #1
    
    Shell EmulatorPath & " " & App.Path & "\MazeRunTestLevel.sna", vbNormalFocus
    

End Sub
