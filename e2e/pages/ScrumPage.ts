import { Page, Locator } from '@playwright/test';

export class ScrumPage {
    readonly page: Page;

    constructor(page: Page) {
        this.page = page;
    }

    async goto() {
        await this.page.goto('/SCRUM.html', { timeout: 60000 });
    }

    async accessScrumView() {
        await this.page.getByRole('link', { name: 'Scrum View' }).click();
        await this.page.waitForURL(/SCRUM\.html/);
    }

    async clickTask(taskName: string) {
        // Find all TODO spans
        const todoSpans = await this.page.locator('span.org-todo').all();
        for (const span of todoSpans) {
            // Get the whole line text from the parent 'pre' or the line itself
            const lineText = await span.evaluate(node => {
                const container = node.closest('pre');
                if (!container) return node.parentElement?.innerText || '';
                const fullText = container.innerText;
                const range = document.createRange();
                range.setStart(container, 0);
                range.setEnd(node, 0);
                const offset = range.toString().length;
                const start = fullText.lastIndexOf('\n', offset) + 1;
                const end = fullText.indexOf('\n', offset);
                return fullText.substring(start, end !== -1 ? end : fullText.length);
            });

            if (lineText.includes(taskName)) {
                // Check if this line actually contains the priority tag if expected
                const hasPriorityA = lineText.includes('[#A]');
                const hasPriorityB = lineText.includes('[#B]');
                const hasPriorityC = lineText.includes('[#C]');

                if (taskName.includes('dolore') && !hasPriorityA) continue;
                if (taskName.includes('Priority B') && !hasPriorityB) continue;
                if (taskName.includes('Priority C') && !hasPriorityC) continue;

                // Found the right one
                const prioritySpan = await span.locator('xpath=following-sibling::span[contains(@class, "org-priority")][1]');
                if (await prioritySpan.isVisible()) {
                    await prioritySpan.click();
                } else {
                    await span.click();
                }
                await this.page.waitForURL(/ORG-TASK\.html/, { timeout: 10000 });
                return;
            }
        }
        throw new Error(`Could not find task "${taskName}"`);
    }
}
