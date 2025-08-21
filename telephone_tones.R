############################################################################
# overview

# turn any sequence of numbers into their telephone tones

# input: a phone number of any length, fed to encode() as a string
#        ex: s <- encode("9924288")
# output: a wav file that plays the sound this number makes

############################################################################
# install packages

# install.packages("seewave")  # run this line once to install the packages
library(seewave)  # synth() spectro() savewav()

############################################################################
# create the codebook (using the old-skool DTMF keypad frequencies)

# the sound for each number on a telephone dial is composed of two tones
# make a dataframe for each number and its two tones (high and low) in Hz

number <- seq(0,9)  # for numbers 0-9 on a telephone dial
low <- c(941,rep(697,3),rep(770,3),rep(852,3))  # tone frequency in Hz
high <- c(1336,rep(c(1209,1336,1477),3))        # tone frequency in Hz

code <- data.frame(number,low,high)
code

############################################################################
# my functions

# create the 2-tone waveform for each number
create_tone <- function(digit,samplingHz,seconds,verbose=FALSE) {
	
	# create two sine waves with synth(sampling_Hz,duration_in_sec,carrier_Hz)
	f1 <- synth(samplingHz,seconds,subset(code,number==digit)$low)
	f2 <- synth(samplingHz,seconds,subset(code,number==digit)$high)
	
	# add the two sine waves together into one waveform
	f3 <- f1+f2
	
	if (verbose == TRUE) {
		par(mfrow=c(3,1))
		plot(head(f1,1000),type="l",las=1,ylab="amplitude",xlab="time",main="low tone")
		plot(head(f2,1000),type="l",las=1,ylab="amplitude",xlab="time",main="high tone")
		plot(head(f3,1000),type="l",las=1,ylab="amplitude",xlab="time",main="both tones")
	
		# check that waveform length = samplingHz * seconds
		if (length(f1) == samplingHz*seconds & 
			length(f2) == samplingHz*seconds & 
			length(f3) == samplingHz*seconds) {
			print(TRUE)
		}	
	}

	return(f3)
}
# example usage:
s <- create_tone(8,44100,0.1)
s <- create_tone(8,44100,0.1,verbose=TRUE)

# create a waveform for any string of numbers
encode <- function(number_string) {
	
	samplingHz <- 44100  # number of sampled values per second
	seconds <- 0.3 # duration of the generated tone in seconds
	space <- 0.1  # the silent space between tones in seconds
	
	w <- c()
	for (i in 1:nchar(number_string)) {
		n <- as.numeric(substr(number_string,i,i))
		print(n)
		s <- create_tone(n,samplingHz,seconds)
		w <- c(w,s,rep(0,samplingHz*space))
	}
	
	return(w)
}
# example usage:
s <- encode("9924288")

############################################################################
# do the thang 

# create the waveform from the number
s <- encode("9924288")

# look at the spectrogram for funzies
spectro(s,44100,flim=c(0,5))

# save the waveform as a .wav file in your current directory
savewav(s, 44100, channel = 1, filename = "992-4288.wav")

############################################################################
# END
############################################################################

