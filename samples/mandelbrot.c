/*
 *  To compile and run:
 *    $ gcc -O3 -o mandelbrot mandelbrot.c
 *    $ ./mandelbrot > mandelbrot.pgm
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define MIN(a, b) (((a) < (b)) ? (a) : (b))

int main(int argc, char** argv)
{
    int i, m;
    int* xs;
    struct timeval start_time, end_time;

    // Allocate memory.
    xs = (int*)malloc(2048*2048*sizeof(int));
    if (!xs) exit(-1);

    // Get start time.
    gettimeofday(&start_time, NULL);

    // Compute Mandelbrot set.
    for (i = 0; i < 2048*2048; ++i) {
        double x = 0.0, y = 0.0;
        double a = (double)(i % 2048 - 512) / 1024.0;
        double b = (double)(i / 2048 - 1024) / 1024.0;
        xs[i] = 0;
        for (m = 1; m < 100; ++m) {
            double x1 = x*x - y*y - a;
            double y1 = 2*x*y - b;
            if (x1*x1 + y1*y1 > 4.0) {
                xs[i] = m;
                break;
            }
            x = x1; y = y1;
        }
    }

    // Get end time and show elapsed time.
    gettimeofday(&end_time, NULL);
    double sec = (double)(end_time.tv_sec - start_time.tv_sec);
    double micro = (double)(end_time.tv_usec - start_time.tv_usec);
    double passed = sec + micro / 1000.0 / 1000.0;
    fprintf( stderr, "Elapsed: %f [sec]\n", passed );
    
    // Output Mandelbrot set in PGM format.
    printf("P2\n");
    printf("2048 2048\n");
    printf("255\n");
    for (i = 0; i < 2048*2048; ++i) {
        printf("%d\n", MIN(255, xs[i]*8));
    }

    // Free memory.
    free(xs);

    return 0;
}
