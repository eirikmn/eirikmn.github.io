import { defineCollection, z } from 'astro:content';
import { glob } from 'astro/loaders';

const publications = defineCollection({
  loader: glob({ pattern: "**/*.md", base: "./src/content/publications" }),

  schema: z.object({
    title: z.string(),
    authors: z.string(),
    year: z.number(),
    venue: z.string(),
    doi: z.string().optional(),
    pdf: z.string().optional(),
  }),
});

export const collections = {
  publications,
};