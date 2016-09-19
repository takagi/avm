/*
 *  To compile and run:
 *    $ gcc -O3 -o nbody nbody.c -lm
 *    $ ./nbody > nbody.pgm
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>

#define SWAP(x, y, T) do { T SWAP = x; x = y; y = SWAP; } while (0)

struct float4 {
    float x, y, z, w;
};

float rsqrt(float x){
    // Use sqrt, not sqrtf, for fair comparison with SBCL.
    return 1.0f / sqrt(x);
}

struct float4 acceleration1(struct float4 xi, struct float4 xj)
{
    struct float4 r, a;
    float dist_sqr, inv_dist, inv_dist_cube, scale;

    r.x = xj.x - xi.x;
    r.y = xj.y - xi.y;
    r.z = xj.z - xi.z;
    r.w = 0.0f;

    dist_sqr = r.x * r.x + r.y * r.y + r.z * r.z + r.w * r.w
             + 0.1f * 0.1f;     /* add softening factor */
    inv_dist = rsqrt(dist_sqr);
    inv_dist_cube = inv_dist * inv_dist * inv_dist;
    scale = xj.w * inv_dist_cube;

    a.x = r.x * scale;
    a.y = r.y * scale;
    a.z = r.z * scale;
    a.w = r.w * scale;

    return a;
}

struct float4 acceleration(struct float4 xi, struct float4* xs0, int n)
{
    int j;
    struct float4 a = { 0.0f, 0.0f, 0.0f, 0.0f };
    struct float4 a1, xj;

    for (j = 0; j < n; ++j) {
        xj = xs0[j];
        a1 = acceleration1(xi, xj);
        a.x += a1.x;
        a.y += a1.y;
        a.z += a1.z;
        a.w += a1.w;
    }

    return a;
}

void integrate_bodies(struct float4* xs, struct float4* xs0, struct float4* vs,
                      int n, float dt)
{
    int i;
    struct float4 a, x;

    for (i = 0; i < n; ++i) {
        x = xs0[i];
        a = acceleration(x, xs0, n);

        vs[i].x += a.x * dt;
        vs[i].y += a.y * dt;
        vs[i].z += a.z * dt;
        vs[i].w += a.w * dt;

        xs[i].x = x.x + vs[i].x * dt; 
        xs[i].y = x.y + vs[i].y * dt; 
        xs[i].z = x.z + vs[i].z * dt; 
        xs[i].w = x.w + vs[i].w * dt;
    }

    return;
}

void init_rand() {
    time_t t;
    srand((unsigned)time(&t));
    return;
}

float random_(float min, float max) {
    return (float)rand() / RAND_MAX * (max - min) + min;
}

void initialize(struct float4* xs, struct float4* vs, int n)
{
    int i;

    for (i = 0; i < n; ++i) {
        xs[i].x = random_(-3.0f, 3.0f);
        xs[i].y = random_(-3.0f, 3.0f);
        xs[i].z = random_(-3.0f, 3.0f);
        xs[i].w = 1.0f;
        vs[i].x = vs[i].y = vs[i].z = vs[i].w = 0.0f;
    }

    return;
}

void plot_body(int* img, int x, int y)
{
    int i, j;

    for (j = y - 5; j < y + 5; ++j) {
        if (0 <= j && j < 1024) {
            for (i = x - 5; i < x + 5; ++i ) {
                if (0 <= i && i < 1024) {
                    img[i + j * 1024] = 255;
                }
            }
        }
    }

    return;
}

void output_bodies(struct float4* xs, int n)
{
    int* img;
    int i, j, k, x, y;

    // Allocate image map.
    img = (int*)malloc(1024 * 1024 * sizeof(int));
    if (!img) exit(-1);

    // Make image map from position.
    for (i = 0; i < n; ++i) {
        x = (int)((xs[i].x + 1.0f) * 512.0f);
        y = (int)((xs[i].y + 1.0f) * 512.0f);
        plot_body(img, x, y);
    }

    // Output image map in PGM format.
    printf("P2\n");
    printf("1024 1024\n");
    printf("255\n");
    for (j = 0; j < 1024; ++j) {
        for (i = 0; i < 1024; ++i) {
            printf("%d ", img[i + j * 1024]);
        }
        printf("\n");
    }

    // Free image map.
    free(img);

    return;
}

int main(int argc, char** argv)
{
    int i, j, k;
    int n = 32768;
    struct float4 *xs, *xs0, *vs, *tmp;
    float dt = 0.001f;
    struct timeval start_time, end_time;
    int* img;

    // Allocate memory.
    xs  = (struct float4*)malloc(n * sizeof(struct float4));
    if (!xs) exit(-1);
    
    xs0 = (struct float4*)malloc(n * sizeof(struct float4));
    if (!xs0) exit(-1);

    vs  = (struct float4*)malloc(n * sizeof(struct float4));
    if (!vs) exit(-1);

    // Initialize position and velocity.
    init_rand();
    initialize(xs, vs, n);

    // Get start time.
    gettimeofday(&start_time, NULL);

    // Simulate bodies.
    for (i = 0; i < 10; ++i) {
        SWAP(xs, xs0, struct float4*);
        integrate_bodies(xs, xs0, vs, n, dt);
    }

    // Get end time and show elapsed time.
    gettimeofday(&end_time, NULL);
    double sec = (double)(end_time.tv_sec - start_time.tv_sec);
    double micro = (double)(end_time.tv_usec - start_time.tv_usec);
    double passed = sec + micro / 1000.0 / 1000.0;
    fprintf(stderr, "Elapsed: %f [sec]\n", passed);
    
    // Output bodies in PGM format.
    output_bodies(xs, n);

    // Free memory.
    free(xs);
    free(xs0);
    free(vs);

    return 0;
}
