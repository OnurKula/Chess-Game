program chess
implicit none
integer:: a(8,8),i,j,k,b
integer:: i1,i2,j1,j2,sayi
character:: hamle*6
open(11,file="tahta.dat.f95")
!reading the chess board and writing it to the screen
sayi=0
do i=1,8
read(11,*) (a(i,j),j=1,8)
write(*,*) (a(i,j),j=1,8)
end do
!Entering the move coordinates for the game by asking
 do k=1,100
if(mod(sayi,2)==0)then
print*,"White Plays"
else
print*,"Black Plays"
end if
50 print*,"piyon:1,kale:2,at:3,fil:4,vezir:5,sah:6"
read(*,*) b
print*,"Which piece do you want to move? (i1,j1)"
read(*,*) i1,j1
print*,"Where do you want to move (i2,j2)"
read(*,*) i2,j2
hamle="dogru"
if(mod(sayi,2)==0)then
if(b==1)then
call piyon(a,i1,j1,i2,j2,hamle)
elseif(b==3)then
call at(a,i1,j1,i2,j2,hamle)
elseif(b==2)then
call kale(a,i1,j1,i2,j2,hamle)
elseif(b==6)then
call sah(a,i1,j1,i2,j2,hamle)
elseif(b==4)then
call fil(a,i1,j1,i2,j2,hamle)
elseif(b==5)then
call vezir(a,i1,j1,i2,j2,hamle)
end if
else
if(b==1)then
call piyon2(a,i1,j1,i2,j2,hamle)
elseif(b==3)then
call at2(a,i1,j1,i2,j2,hamle)
elseif(b==2)then
call kale2(a,i1,j1,i2,j2,hamle)
elseif(b==6)then
call sah2(a,i1,j1,i2,j2,hamle)
elseif(b==4)then
call fil2(a,i1,j1,i2,j2,hamle)
elseif(b==5)then
call vezir2(a,i1,j1,i2,j2,hamle)
end if
end if

if(hamle=="yanlis")then
go to 50
else
sayi=sayi+1
end if

!writing the latest situation to the screen as a result of the move made
do i=1,8
write(*,*) (a(i,j),j=1,8)
end do
 end do
end program 

subroutine piyon(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark
character:: hamle*6

ifark=i2-i1
jfark=j2-j1

!pawn ​​rules
if(a(i1,j1)== 1 .and. a(i2,j2)== 0 .and. ifark==1 .and. jfark==0)then
a(i2,j2)=1
a(i1,j1)=0
!pawn ​​eating a piece
elseif(a(i1,j1)==1 .and. a(i2,j2)<0 .and. ifark==1 .and. abs(jfark)==1)then
a(i2,j2)=1
a(i1,j1)=0
else
print*,"Yanlis hamle"
hamle="yanlis"
return
end if
end subroutine


subroutine at(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark
character:: hamle*6
ifark=i2-i1
jfark=j2-j1

!knight rules
!While the knight was playing in vain
if(a(i1,j1)== 3 .and. a(i2,j2)== 0 .and. abs(ifark)==2 .and.abs(jfark)==1)then
a(i2,j2)=3
a(i1,j1)=0
elseif(a(i1,j1)==3 .and. a(i2,j2)==0 .and. abs(ifark)==1 .and. abs(jfark)==2)then
a(i2,j2)=3
a(i1,j1)=0
!at taş yerken
elseif(a(i1,j1)==3 .and. a(i2,j2)<0 .and. abs(ifark)==2 .and. abs(jfark)==1)then
a(i2,j2)=3
a(i1,j1)=0
elseif(a(i1,j1)==3 .and. a(i2,j2)<0 .and. abs(ifark)==1 .and. abs(jfark)==2)then
a(i2,j2)=3
a(i1,j1)=0
else
print*,"Yanlis hamle"
hamle="yanlis"
return
end if
end subroutine

subroutine kale(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark,k
character:: hamle*6

ifark=i2-i1
jfark=j2-j1
!düz giderken
if(i2>i1)then
do k=i1+1,i2-1
if(a(k,j1).ne.0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo
!going to the right
elseif(j2>j1 .and. i1==i2)then
do k=j1+1,j2-1
if(a(i1,k).ne.0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo
!while going back
elseif(i1>i2 .and. j1==j2)then
do k=i1-1,i2+1,-1
if(a(k,j1)/=0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo
!going left
elseif(j1>j2 .and. i1==i2 )then
do k=j1-1,j2+1,-1
if(a(i1,k).ne.0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo
end if



if(a(i1,j1)==2.and.a(i2,j2)<=0.and.ifark==0.and.abs(jfark)>=1.and.abs(jfark)<=7)then
a(i2,j2)=2
a(i1,j1)=0
elseif(a(i1,j1)==2.and.a(i2,j2)<=0.and.abs(ifark)>=1.and.abs(ifark)<=7.and.jfark==0)then
a(i2,j2)=2
a(i1,j1)=0
else
print*, 'Yanlis hamle'
hamle="yanlis"
return
endif

end subroutine

subroutine sah(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark,k
character:: hamle*6
ifark=i2-i1
jfark=j2-j1
if(a(i1,j1)==6.and.a(i2,j2)<=0.and. abs(ifark)==1 .and.abs(jfark)==0 .or. ifark==0 .and. abs(jfark)==1 .or. abs(ifark)==1 .and. &
abs(jfark)==1)then
a(i2,j2)=6
a(i1,j1)=0
else
print*,"wrong move"
hamle="yanlis"
end if

end subroutine

subroutine fil(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark,k,b,c
character:: hamle*6
ifark=i2-i1
jfark=j2-j1
b=j1+1
c=j1-1
!straight right cross
if(j2>j1 .and. i2>i1)then
do k=i1+1,i2-1
if(a(k,b)/=0)then
b=b+1
print*,"There are stones on the road"
hamle="yanlis"
return
end if
end do

!straight left diagonal
elseif(j2<j1 .and. i2>i1)then
do k=i1+1,i2-1
if(a(k,c)/=0)then
c=c-1
print*,"There are stones on the road"
hamle="yanlis"
return
end if
end do

!back right cross
elseif(j2>j1 .and. i1>i2)then
do k=i1-1,i2+1,-1
if(a(k,b)/=0)then
b=b+1
print*,"There are stones on the road"
hamle="yanlis"
return
end if
end do

!back left diagonal
elseif(j2<j1 .and. i1>i2)then
do k=i1-1,i2+1,-1
if(a(k,c)/=0)then
c=c-1
print*,"There are stones on the road"
hamle="yanlis"
return
end if
end do
end if


if(a(i1,j1).eq.4.and.a(i2,j2)<=0.and.abs(ifark).eq.abs(jfark))then
a(i2,j2)=4
a(i1,j1)=0
else 
print*,"wrong move"
hamle="yanlis"
end if

end subroutine

subroutine vezir(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark,k,b,c
character:: hamle*6

ifark=i2-i1
jfark=j2-j1
b=j1+1
c=j1-1


!straight right cross
if(j2>j1 .and. i2>i1)then
do k=i1+1,i2-1
if(a(k,b)/=0)then
b=b+1
print*,"There are stones on the road"
hamle="yanlis"
return
end if
end do

!straight diagonal left
elseif(j1>j2 .and. i2>i1  )then
do k=i1+1,i2-1
if(a(k,c)/=0)then
c=c-1
print*,"There are stones on the road"
hamle="yanlis"
return
end if
end do

!back right cross
elseif(j2>j1 .and. i1>i2)then
do k=i1-1,i2+1,-1
if(a(k,b)/=0)then
b=b+1
print*,"There are stones on the road"
hamle="yanlis"
return
end if
end do

!back left diagonal
elseif(j1>j2 .and. i1>i2)then
do k=i1-1,i2+1,-1
if(a(k,c)/=0)then
c=c-1
print*,"There are stones on the road"
hamle="yanlis"
return
end if
end do


!while going straight
elseif(i2>i1 .and. j1==j2)then
do k=i1+1,i2-1
if(a(k,j1).ne.0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo

!going to the right
elseif(j2>j1 .and. i1==i2)then
do k=j1+1,j2-1
if(a(i1,k).ne.0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
end do

!while going back
elseif(i1>i2 .and. j1==j2)then
do k=i1-1,i2+1
if(a(k,j1).ne.0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo

!going left
elseif(j1>j2 .and. i1==i2)then
do k=j1-1,j2+1
if(a(i1,k).ne.0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo
end if






if(a(i1,j1)==5.and.a(i2,j2)<=0.and.ifark==0.and.abs(jfark)>=1.and.abs(jfark)<=7 .or. &
a(i1,j1)==5.and.a(i2,j2)<=0.and.abs(ifark).eq.abs(jfark))then
a(i2,j2)=5
a(i1,j1)=0
elseif(a(i1,j1)==5.and.a(i2,j2)<=0.and.abs(ifark)>=1.and.abs(ifark)<=7.and.jfark==0 .or. &
a(i1,j1)==5.and.a(i2,j2)<=0.and.abs(ifark).eq.abs(jfark))then
a(i2,j2)=5
a(i1,j1)=0
else
print*, "Wrong move"
hamle="yanlis"
return
endif

end subroutine
                        
subroutine piyon2(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark
character:: hamle*6

ifark=i2-i1
jfark=j2-j1


!pawn ​​rules
if(a(i1,j1)== -1 .and. a(i2,j2)== 0 .and. ifark==-1 .and. jfark==0)then
a(i2,j2)=-1
a(i1,j1)=0
!pawn ​​eating a piece
elseif(a(i1,j1)==-1 .and. a(i2,j2)>0 .and. ifark==-1 .and. abs(jfark)==1)then
a(i2,j2)=-1
a(i1,j1)=0
else
print*,"Wrong move"
hamle="yanlis"
return
end if
end subroutine

subroutine at2(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark
character:: hamle*6
ifark=i2-i1
jfark=j2-j1

!knight rules
!While the knight was playing in vain
if(a(i1,j1)== -3 .and. a(i2,j2)== 0 .and. abs(ifark)==2 .and.abs(jfark)==1)then
a(i2,j2)=-3
a(i1,j1)=0
elseif(a(i1,j1)==-3 .and. a(i2,j2)==0 .and. abs(ifark)==1 .and. abs(jfark)==2)then
a(i2,j2)=-3
a(i1,j1)=0

!knight eating stones
elseif(a(i1,j1)==-3 .and. a(i2,j2)>0 .and. abs(ifark)==2 .and. abs(jfark)==1)then
a(i2,j2)=3
a(i1,j1)=0
elseif(a(i1,j1)==-3 .and. a(i2,j2)>0 .and. abs(ifark)==1 .and. abs(jfark)==2)then
a(i2,j2)=-3
a(i1,j1)=0
else
print*,"Wrong move"
hamle="yanlis"
return
end if
end subroutine





subroutine fil2(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark,k,b,c
character:: hamle*6
ifark=i2-i1
jfark=j2-j1
b=j1+1
c=j1-1

!straight right cross
if(j2>j1.and.i2>i1)then
do k=i1+1,i2-1
if(a(k,b)/=0)then
b=b+1
print*,"there are stones on the road"
hamle="yanlis"
return
end if
end do

!straight left diagonal
elseif(j2<j1.and.i2>i1)then
do k=i1+1,i2-1 
if(a(k,c)/=0)then
c=c-1
print*,"there are stones on the road"
hamle="yanlis"
return
end if
end do

!back right cross
elseif(j2>j1 .and. i1>i2)then
do k=i1-1,i2+1,-1
if(a(k,b)/=0)then
b=b+1
print*,"there are stones on the road"
hamle="yanlis"
return
end if
end do

!back left diagonal
elseif(j2<j1 .and. i1>i2)then
do k=i1-1,i2+1,-1
if(a(k,c)/=0)then
c=c-1
print*,"there are stones on the road"
hamle="yanlis"
return
end if
end do
end if




if(a(i1,j1)==-4.and.a(i2,j2)>=0.and.abs(ifark).eq.abs(jfark))then
a(i2,j2)=-4
a(i1,j1)=0
else
print*,"Wrong move"
hamle="yanlis"
return
end if
end subroutine




subroutine kale2(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark,k
character:: hamle*6
ifark=i2-i1
jfark=j2-j1

!while going straight
if(i2>i1)then
do k=i1+1,i2-1
if(a(k,j1)/=0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo

!going to the right
elseif(j2>j1 .and. i1==i2)then 

do k=j1+1,j2-1
if(a(i1,k)/=0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo


!while going back
elseif(i1>i2 .and. j1==j2)then
do k=i1-1,i2+1,-1
if(a(k,j1)/=0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo

!going left
elseif(j1>j2 .and. i1==i2 )then
do k=j1-1,j2+1,-1
if(a(i1,k).ne.0)then
print*,"There are stones on the road"
hamle="yanlis"
return
endif
enddo
end if


!CASTLE RULES
if(a(i1,j1)==-2.and.a(i2,j2)>=0.and.ifark==0.and.abs(jfark)>=1.and.abs(jfark)<=7)then
a(i2,j2)=-2
a(i1,j1)=0
elseif(a(i1,j1)==-2.and.a(i2,j2)>=0.and.abs(ifark)>=1.and.abs(ifark)<=7.and.jfark==0)then
a(i2,j2)=-2
a(i1,j1)=0
else
print*, 'Wrong move'
hamle="yanlis"
return
endif

end subroutine




subroutine sah2(a,i1,j1,i2,j2,hamle)
implicit none
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark,k
character:: hamle*6
ifark=i2-i1
jfark=j2-j1
if(a(i1,j1)==-6.and.a(i2,j2)>=0.and. abs(ifark)==1 .and.abs(jfark)==0 .or. &
ifark==0 .and. abs(jfark)==1 .or. abs(ifark)==1 .and.  abs(jfark)==1)then
a(i2,j2)=-6
a(i1,j1)=0
else
print*,"Wrong move"
hamle="yanlis"
end if

end subroutine




subroutine vezir2(a,i1,j1,i2,j2,hamle)
integer:: a(8,8),i1,i2,j1,j2,ifark,jfark,k,b,c
character:: hamle*6
ifark=i2-i1
jfark=j2-j1

b=j1+1
c=j1-1

!straight right cross
if(j2>j1 .and. i2>i1)then
do k=i1+1,i2-1
if(a(k,b)/=0)then
b=b+1
print*,"there are stones on the road"
hamle="yanlis"
return
end if
end do

!straight diagonal left
elseif(j1>j2 .and. i2>i1  )then
do k=i1+1,i2-1
if(a(k,c)/=0)then
c=c-1
print*,"there are stones on the road"
hamle="yanlis"
return
end if
end do

!back right cross
elseif(j2>j1 .and. i1>i2)then
do k=i1-1,i2+1,-1
if(a(k,b)/=0)then
b=b+1
print*,"there are stones on the road"
hamle="yanlis"
return
end if
end do

!back left diagonal
elseif(j1>j2 .and. i1>i2)then
do k=i1-1,i2+1,-1
if(a(k,c)/=0)then
c=c-1
print*,"there are stones on the road"
hamle="yanlis"
return
end if
end do

!While going straight
elseif(i2>i1 .and. j1==j2)then
do k=i1+1,i2-1
if(a(k,j1).ne.0)then
print*,"there are stones on the road"
hamle="yanlis"
return
endif
enddo

!going to the right
elseif(j2>j1 .and. i1==i2)then
do k=j1+1,j2-1
if(a(i1,k).ne.0)then
print*,"there are stones on the road"
hamle="yanlis"
return
endif
end do

!while going back
elseif(i1>i2 .and. j1==j2)then
do k=i1-1,i2+1
if(a(k,j1).ne.0)then
print*,"there are stones on the road"
hamle="yanlis"
return
endif
enddo

!going left
elseif(j1>j2 .and. i1==i2)then
do k=j1-1,j2+1
if(a(i1,k).ne.0)then
print*,"there are stones on the road"
hamle="yanlis"
return
endif
enddo
end if





if(a(i1,j1)==-5.and.a(i2,j2)>=0.and.ifark==0.and.abs(jfark)>=1.and.abs(jfark)<=7.or. &
a(i1,j1)==-5.and.a(i2,j2)>=0.and.abs(ifark).eq.abs(jfark))then
a(i2,j2)=-5
a(i1,j1)=0
elseif(a(i1,j1)==-5.and.a(i2,j2)>=0.and.abs(ifark)>=1.and.abs(ifark)<=7.and.jfark==0 .or. &
a(i1,j1)==-5.and.a(i2,j2)>=0.and.abs(ifark).eq.abs(jfark))then
a(i2,j2)=-5
a(i1,j1)=0
else
print*, "Wrong move"
hamle="yanlis"
return
endif

end subroutine













