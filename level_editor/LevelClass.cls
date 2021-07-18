VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "LevelClass"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

    Public Name As String '* 12
    Public Password As String
    
    Public BackCol As Byte
    
    Public GemsNeeded As Byte
    
    Dim Teleport(5) As Byte

    Dim Map(11, 11) As Byte

Public Sub SetTile(X As Long, Y As Long, T As Long)
    
    If X < 0 Or Y < 0 Or X > 11 Or Y > 11 Then Exit Sub
    If T < 0 Or T > 255 Then Exit Sub
    
    Map(X, Y) = T

End Sub

Public Function GetTile(X As Long, Y As Long) As Byte

    If X < 0 Or Y < 0 Or X > 11 Or Y > 11 Then Exit Function

    GetTile = Map(X, Y)

End Function

Public Sub PutTeleport(X As Long, Y As Long, I As Long)

    If X < 0 Or X > 11 Or Y < 0 Or Y > 11 Or I < 0 Or I > 5 Then Exit Sub
    Teleport(I) = Y * 16 + X

End Sub

Public Sub PutTeleportByte(B As Byte, I As Long)

    If I < 0 Or I > 5 Then Exit Sub
    Teleport(I) = B

End Sub

Public Function GetTeleportByte(I As Long) As Byte

    If I < 0 Or I > 5 Then Exit Function

    GetTeleportByte = Teleport(I)

End Function

Public Function GetTeleportX(I As Long)

    If I < 0 Or I > 5 Then Exit Function
    
    GetTeleportX = (Teleport(I) And 15)

End Function

Public Function GetTeleportY(I As Long)

    If I < 0 Or I > 5 Then Exit Function
    
    GetTeleportY = (Teleport(I) And 240) / 16

End Function
