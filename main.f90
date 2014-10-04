! *************** Documentation notes *********************
! Author: Freddy Rodrigo Mendoza Ticona (UNI-FIEE,IPEN-verano2014)
! interfaz de adquisicion de datos del encoder AMT 102 con un texas instruments TIVA launchpad.
! fecha: 13/03/2014 
! correo: freddy12120@gmail.com
! Afiliacion: 
! -Universidad Nacional de Ingenieria - UNI 
!**********************************************************

	Program AppEncoderv20
	Use Winteracter
	Use Resource
	Use SCCINF


     implicit none   
   
!--- Variables de manejo de eventos y mensajes de Winteracter ---!
	integer		:: i,j
	 

	INTEGER(4) 	:: PORT, BAUD, PARITY, STPBTS, CHRLEN, FLOW, STATUS,RLEN, SLEN, CODE, NBYTES, LENGHT, LSREG,MSREG,ERRSTS,&
	  															 RECCNT,SNDCNT,RECLEN,SNDLEN 
	integer		:: m ! index 
	
	character(1),dimension(64)			:: buffer = ""
	character(len=50000)				:: tempo,getstring 
        character(len=1000000)       	                :: string_volta
	
	character(len=10),dimension(50000) 			:: Tmseg_c
	character(len=10),dimension(50000) 			:: Tseg_c
	integer, dimension(50000)  				:: Tmseg,Tseg
	real,dimension(50000)  					:: tiempo_plot  
	character(len=20), dimension(50000)		    	:: tiempo_str
	character(len=10), dimension(50000)		    	:: contador_str
	real :: grados_girados
	character(len = 20)					:: grados_girados_str
	
	integer :: IPOSITION
	integer :: jvolta = 1
	integer	:: i_volta
	integer :: lenvolta
	integer :: evento
	integer :: start_first =  0 
    
    type(win_message)          					:: mensaje  
    integer						        :: popen = 0
	
    integer :: iread=1
    tempo  = ""
    string_volta = ""
    lenvolta =  1
	
	
	
	  !****************Parameters save file************************
     character(len=5000):: NAME_file
     NAME_file = ""
   
	
	
	
	!*************PLOT VARIABLES****************************
	
      INTEGER, PARAMETER, DIMENSION(3)     :: ICOLAXES = (/128+256*128+256*256*128, &
                                                           160+256*160+256*256*160, &
                                                           192+256*192+256*256*192/)
      
      REAL,               DIMENSION(24,4)  :: ADATA,RDATA,RDATA2
      CHARACTER(LEN=6)  , DIMENSION(10)    :: LABELS
      INTEGER, PARAMETER, DIMENSION(10)    :: ICOLOUR  = (/16744576, &
                                                           8421631,  &
                                                           32768,    &
                                                           16744703, &
                                                           15395328, &
                                                           48896,    &
                                                           9743096,  &
                                                           10485760, &
                                                           33023,    &
                                                           10388577/)
      INTEGER,DIMENSION(2)     :: IHWIN

      REAL, PARAMETER :: CHWID = 0.013
      REAL, PARAMETER :: CHHGT = 0.030

      INTEGER :: IPLOTWIN
      LOGICAL :: SOURCE_FOUND
      INTEGER :: NSets,IStyle1,IStyle2,IStyle3,ICol1,ICol2,ISET,NValue,Layout
      REAL    :: XMinValue,YMinValue,ZMinValue,XMaxValue,YMaxValue,ZMaxValue
      CHARACTER(LEN=260) :: FILENAME

   
    
		PORT   	 = 3
		BAUD 	 = 13
		PARITY 	 = 1
		STPBTS 	 = 1
		CHRLEN 	 = 5
		FLOW   	 = 1
		LENGHT 	 = 1   ! londgitud de buffer de recepcion, 1 byte 
		RLEN   	 = 8192	
		SLEN   	 = 8192    
		CODE   	 = 1

   
!---- Inicializacion y Configuracion ---------!
	call WInitialise()
 

!---- Open Main Window ----------------------!
	CALL WindowOpen(HideWindow)
	call WDialogLoad(IDD_DIALOG_MAIN)

	call WDialogShow(-1,-1,0,Semimodeless)
	call WDialogSelect(IDD_DIALOG_MAIN)
						
	
	
	
	!call WDialogFieldState(ID_START,disabled)
	call WDialogFieldState(IDF_PLOT,disabled)
	call WDialogFieldState(IDF_SAVE,disabled)
	
	call IgrSelect(DrawField,IDF_DRAW)  !seleccciona el destino de dibujo
	
	


!----------------------------------------------------------------------------
! **** LAZO PRINCIPAL DEL PROGRAMA ****
!----------------------------------------------------------------------------
	do
	
		
		if (popen==1) then
	 	 	call ReadPortCom()
		end if
		
		
	!call ReadPortCom()
	call WMessagePeek(evento,mensaje)
          SELECT CASE (evento)
          		CASE (PushButton)
                    	  	SELECT CASE (mensaje%value1)
                    	  			
                    	  			
		               		CASE (ID_EXIT)
										 
						buffer(1) =  'A'
					        NBYTES    =  1
														
													
														
														
					        call COMSND(PORT,BUFFER,NBYTES,STATUS)
														
    						call WMessageBox(YesNo,QuestionIcon,CommonNo,"Are you sure?","Exit")
						if (WInfoDialog(ExitButtonCommon)/= CommonNo) then 
						Exit
						end if
						     	
   															
															
   																			
                                        CASE (ID_START) ! init process
									
						CALL WDialogGetRadioButton(IDF_RADIO12,IPOSITION)	
						SELECT CASE (IPOSITION)
							CASE(1)
							   buffer(1) =  'Q'
                                                           buffer(2) =  char(13)
                                                           NBYTES    =   2
                                                           call COMSND(PORT,BUFFER,NBYTES,STATUS)
                                                           if  (start_first == 0) then 
                                                              call COMSND(PORT,BUFFER,NBYTES,STATUS)
                                                              start_first = 1;
                                                           end if
																 
																  
						        CASE(2)
                                                           buffer(1) =  'W'
                                                           buffer(2) =  char(13)
                                                           NBYTES    =   2
                                                           call COMSND(PORT,BUFFER,NBYTES,STATUS)
																  
                                                        CASE(3)
                                                           buffer(1) =  'E'
                                                           buffer(2) =  char(13)
                                                           NBYTES    =   2
                                                           call COMSND(PORT,BUFFER,NBYTES,STATUS)
																  
                                                        CASE(4)
                                                           buffer(1) =  'R'
                                                           buffer(2) =  char(13)
                                                           NBYTES    =   2
                                                           call COMSND(PORT,BUFFER,NBYTES,STATUS)
                                                           
                                                        CASE(5)
                                                           buffer(1) =  'T'
                                                           buffer(2) =  char(13)
                                                           NBYTES    =   2
                                                           call COMSND(PORT,BUFFER,NBYTES,STATUS)
                                                           
                                                        END SELECT
              
                                                        lenvolta  = 1 
                                                        string_volta=""
                                                        call WDialogPutString(IDF_SERIAL,"")
                                                        
                                                        CODE  = 1
                                                        call COMFLS(PORT,CODE,STATUS)! Limpia el buffer de recepcion					 																				  		
														
																					 
                                                     CASE (ID_OPEN)
                                                        call WDialogGetInteger(IDF_PORT,PORT)
                                                        CALL COMOPN(PORT,BAUD,PARITY,STPBTS,CHRLEN,FLOW,RLEN,SLEN,STATUS)
                                                        if (STATUS/=0) then
                                                           call ExitProgram()
                                                        end if
                                                        !call WDialogFieldState(ID_START,Enabled) 
                                                        call WDialogFieldState(ID_OPEN,disabled)
                                                        popen=1
                                                        
                                                     CASE(ID_STOP)
                                                        !call WDialogFieldState(ID_START,disabled)
                                                        buffer(1) =  'A'
                                                        NBYTES    =  1
                                                        call COMSND(PORT,BUFFER,NBYTES,STATUS)
                                                        call WDialogPutString(IDF_SERIAL,"")
                                                        
                                                     CASE(IDF_CLEANTERMINAL)
                                                        call WDialogPutString(IDF_SERIAL,"")
                                                        
                                                     CASE(IDF_PLOT)
                                                        call PGPlot10()
              
                                                     CASE(IDF_GETDATA)
										
                                                        
                                                        call WDialogFieldState(IDF_SAVE,Enabled)
                                                        call WDialogFieldState(IDF_PLOT,Enabled)
                                                        call DecodeData()
                                                        
                                                        call WDialogPutString(IDF_DATASET,grados_girados_str)
                                                        call WDialogGetReal(IDF_DATASET,grados_girados)
                                                        grados_girados = grados_girados * 360/(2048*4)
                                                        
                                                        do i=1,lenvolta
                                                           call WDialogPutString(IDF_DATASET,Tmseg_c(i))
                                                           call WDialogGetInteger(IDF_DATASET,Tmseg(i))
                                                           
                                                           !call WDialogPutString(IDF_DATASET,Tseg_c(i))
                                                           !call WDialogGetInteger(IDF_DATASET,Tseg(i))
                                                           call WDialogPutInteger(IDF_DATASET,i)
                                                           call WDialogGetString(IDF_DATASET,contador_str(i))
                                                           
                                                           tiempo_plot(i) = 25.E-6*real(Tmseg(i)) !+209.712*real(Tseg(i))
                                                           
                                                           call WDialogPutReal(IDF_DATASET,tiempo_plot(i))
                                                           call WDialogGetString(IDF_DATASET,tiempo_str(i))
                                                           
                                                        end do
                                                        call WDialogPutReal(IDF_YAXES,maxval(tiempo_plot(1:lenvolta)))
                                                        call WDialogPutReal(IDF_GRADOSGIRADOS,grados_girados)
                                                        
                                                     CASE(IDF_SAVE)
                                                        call Database()
                                                        call WDialogFieldState(IDF_SAVE,disabled)
                                                        
										

                                                     END SELECT
        
								
!  Expose event - Redraw our graph
!
						!CASE (Expose)
						!			IF (mensaje%VALUE3 == FromDialog .AND. &
						!				 mensaje%VALUE4 == IDF_DRAW) THEN
										
						!			END IF
!  FieldChanged event - Update graph is required when field value changed
!                       Ignore movement between fields
!
						
						!CASE (FieldChanged)
						!			IF (mensaje%VALUE1 == mensaje%VALUE2) THEN
						!					call PGPlot10()
						!		    END IF

      		
                                                  END SELECT
                                               END DO
!-------------------------------------------------------------------
! **** FIN DE LAZO PRINCIPAL DE PROGRAMA
!-------------------------------------------------------------------
     call ExitProgram()


!===================================================================3
! **** SUBRUTINAS ****
!===================================================================  
	
	
	CONTAINS

	
	! decode all data
	
	subroutine DecodeData()
		
		 integer :: i,a,b,c
		 character:: aux
		 integer :: flaginit,flagmedio,flagfinal,flafgrados,flagrados
		 integer  :: len_data
		 flaginit = 0
		 flagfinal = 0
		 flagmedio = 0
		 flagrados  =  0
		 flafgrados  =  0
                 jvolta = 1
		 
		 len_data =  len_trim(string_volta)
	   
   do i=1, len_data
  		 
      aux  = string_volta(i:i)
  		
  		 
      if(aux  == 'A')then 
         a = i
         flaginit = 1
      end if
  		
      if(aux  == char(13))then
         b = i
         if(flaginit == 1)then
            flagmedio = 1
         end if
         
         if(flagrados == 1)then
            flafgrados= 1
         end if
         
      end if
  
      if(aux  == 'N')then 
         a = i
         flagrados  = 1
      end if
		 !if(aux  == char(13))then
  		 !		c = i
		 !		if(flaginit == 1 .AND. flagmedio == 1)then
		 !			flagfinal = 1
		 !		end if
  		 !end if 
		 
  		 !if (flaginit == 1 .AND. flagfinal == 1 .AND. flagmedio == 1)then 
      if (flaginit == 1 .AND. flagfinal == 0 .AND. flagmedio == 1)then 
         lenvolta = jvolta
         call WDialogPutInteger(IDF_NUMDATA,lenvolta)
         Tmseg_c(jvolta)  = string_volta((a+1):(b-1))
         !Tseg_c (jvolta)  = string_volta((b+1):(c-1))
         
         jvolta = jvolta + 1
         flagfinal = 0
         flaginit  = 0 	
         flagmedio = 0				
      end if
		 
      if(flafgrados == 1 .AND. flagrados == 1 )then
         
         grados_girados_str = string_volta((a+1):(b-1))
         flafgrados = 0 
         flagrados = 0
      end if
  		 	
   end do
 end subroutine 

	
	
 SUBROUTINE PGPlot10()
!
!  Simple line plot with markers at each point
!
!  Call IPgNewPlot to start a new plot
!
!  NSets  = Number of data sets to be plotted.
!  NValue = Number of values to plot for each data set.
!  Use the default layout options
!
      NSets  = 1
      NValue = lenvolta !longitud del ploteo
      CALL IPgNewPlot(PgLinePlot,NSets,NValue)
!
!  Set style options
!
!  IStyle1 = Solid Lines
!  IStyle2 = Use markers at each point on the line
!  IStyle3 = Do not fill under the line
!  ICol1   = Line colour
!  ICol2   = Marker Colour
!
      IStyle1 = SolidLine
      IStyle2 = PgMarker
      IStyle3 = Outline
      ICol2   = WRGB(20,40,120)
      
      ICol1 = ICOLOUR(10)
      CALL IPgStyle(ISet,IStyle1,IStyle2,IStyle3,ICol1,ICol2)
      CALL IPgMarker(1,3)
    
!
!  Set appropriate Presentation Graphics units for the plot
!
      call calcXYmaxMin()
      CALL IPGUnits(XMinValue,YMinValue,XMaxValue,YMaxValue)
!
!  Add a title
!
      CALL WGrTextFont(FFTimes,0,0.02,0.04)
      CALL IPgTitle('Plot - AppEncoder')
	  CALL IPgXLabel('Cuentas')
      CALL IPgYLabelLeft('Tiempo (ms)','C9')
!
!  Draw axes
!
      CALL IPgAxes()
!
!  Plot the lines
!
      CALL WGrTextFont(FFCourier,WIDTH=0.016,HEIGHT=0.030)
     
      CALL IPgLinePlot(tiempo_plot(1:lenvolta))
!
!  Draw X and Y scales at bottom and left, with tick marks and numbering
!
      CALL WGrTextFont(FFCourier,WIDTH=CHWID,HEIGHT=CHHGT)
      CALL IPgXScale('TN')
      CALL IPgYScaleLeft('TN')
!

!
     
!
!  Restore defaults
!
      CALL IPgMarker(1,-1)
      CALL IPgMarker(2,-1)
      RETURN
      END SUBROUTINE PGPlot10
	  
	  SUBROUTINE cleanPlot()
		CALL WBitmapDestroy(IDF_DRAW)
			
	  
	  END SUBROUTINE 
	  
	  
	  
	  Subroutine calcXYmaxMin()
	    
		XMinValue =   0.0
		XMaxValue =   real(lenvolta) 
		YMinValue =   0.0
		call WDialogGetReal(IDF_YAXES,YMaxValue)
	
	  end Subroutine

	Subroutine ExitProgram()
          CALL COMCLS(PORT,1,STATUS)
          call WDialogUnload()
          CALL WindowClose()
        Stop
	End Subroutine ExitProgram
	
	Subroutine ReadPortCom()
	
   		NBYTES = 0  
  		LENGHT = 60
   		call COMREC(PORT,BUFFER,LENGHT,LSREG,MSREG,NBYTES,STATUS)
   		if (STATUS/=0) then
   				call ExitProgram()
   		end if
   		
  ! muestra en el textbox 		
   		 if (NBYTES>0) then
   				tempo = ""
   			do m = 1, NBYTES
   				tempo(m:m) = buffer(m) 							
   			end do

				string_volta = trim(string_volta)//trim(tempo)	
			
   				call WDialogGetString(IDF_SERIAL,getstring)
   				
   				call WdialogPutString(IDF_SERIAL,trim(getstring)//trim(tempo))
   		 end if
	End Subroutine ReadPortCom
	
	subroutine Database()
		character (len=30) :: palabra,palabra2
		integer :: idata 
		palabra = 'Cuentas 	    tiempo(ms)'
		palabra2 = 'FIN'
		
		
		CALL WSelectFile(FILTERSTR='text file (.txt)|*.txt|',   					 	&
                     IFLAGS   =SaveDialog+NonExPath+AppendExt, 					    &
                     FILEDIR  =NAME_file,           										    &
                     TITLE    ='Select file')
																				 
		
       if (WInfoDialog(ExitButtonCommon)==CommonOK) then
           open(76,file=trim(NAME_file),delim='None') ! crea el archivo 
           write(76,*)palabra//char(13)//char(10)
           
           do idata = 1,lenvolta
              
              write(76,*) contador_str(idata)//"               "//tiempo_str(idata) ! almacena todos los datos en un archivo .txt
		
           end do
           write(76,*)palabra2//char(13)//char(10)  ! fin de sesion
           close (unit=76, status='keep') ! cierra la conexion con el archivo 
        end if
										
      end subroutine 
      
END
